#![feature(proc_macro_hygiene, decl_macro)]
use self::diesel::prelude::*;

#[macro_use]
extern crate rocket;
use rocket::response::NamedFile;
use std::path::{Path, PathBuf};
extern crate chrono;
#[macro_use]
extern crate diesel;
#[macro_use]
extern crate rocket_contrib;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
use rocket_contrib::json::Json;
pub mod cors;
pub mod models;
pub mod schema;
pub mod types;
use chrono::NaiveDateTime;
use diesel::insert_into;
use image::ImageFormat;
use models::{Image, Post};
use rocket::http::ContentType;
use rocket::http::Status;
use rocket::response::Content;
use rocket::response::Stream;
use rocket::Data;
use std::io::Cursor;
use std::time::{SystemTime, UNIX_EPOCH};
use types::{Admin, CachedFile, FileName};

#[database("newsroom")]
pub struct DbConn(diesel::PgConnection);

#[get("/")]
fn index() -> Option<CachedFile> {
    NamedFile::open(Path::new("server/public/index.html"))
        .ok()
        .map(|n| CachedFile(n, 0))
}

#[get("/<file..>", rank = 10)]
fn files(file: PathBuf) -> Option<CachedFile> {
    NamedFile::open(Path::new("server/public/").join(file))
        .ok()
        .map(|n| CachedFile(n, 31536000)) //  1 year (24*60*60*365)
        .or_else(|| {
            NamedFile::open(Path::new("server/public/index.html"))
                .ok()
                .map(|n| CachedFile(n, 0))
        })
}

#[post("/newsroom/posts?<i>&<range>")]
pub fn posts(conn: DbConn, i: i64, range: i64) -> Result<Json<Vec<Post>>, String> {
    use crate::schema::posts::dsl::*;

    posts
        .order(posttime.desc())
        .offset(i)
        .limit(range.min(3))
        .load(&conn.0)
        .map_err(|_err| -> String { "Error querying page views from the database".into() })
        .map(Json)
}

#[post("/newsroom/getimages")]
pub fn get_images(conn: DbConn, _admin: Admin) -> Result<Json<Vec<String>>, String> {
    use crate::schema::images::dsl::*;
    match images.order(postat.desc()).load::<Image>(&conn.0) {
        Ok(imgs) => Ok(Json(
            imgs.into_iter()
                .map(|img| img.imagename)
                .collect::<Vec<String>>(),
        )),
        Err(_) => Err("Invalid request".to_string()),
    }
}

#[post("/newsroom/upload/image", data = "<upload>")]
pub fn upload_image(
    conn: DbConn,
    _admin: Admin,
    upload: Data,
    file_name: FileName,
) -> Result<Json<Vec<String>>, String> {
    use crate::schema::images::dsl::*;
    let mut vec = Vec::new();
    upload.stream_to(&mut vec);
    let new_image = image::load_from_memory(&vec).unwrap();
    let mut thumbnail_data = Vec::new();
    new_image
        .thumbnail(100, 100)
        .write_to(
            &mut thumbnail_data,
            if let Ok(format) = ImageFormat::from_path(&file_name.0) {
                image::ImageOutputFormat::from(format)
            } else {
                image::ImageOutputFormat::Unsupported("Invalid image in database".to_string())
            },
        )
        .expect("");
    insert_into(images)
        .values((
            imagename.eq(file_name.0),
            postat.eq(NaiveDateTime::from_timestamp(
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs() as i64,
                0,
            )),
            main.eq(vec),
            thumbnail.eq(thumbnail_data),
        ))
        .execute(&conn.0)
        .unwrap();
    match images.order(postat.desc()).load::<Image>(&conn.0) {
        Ok(imgs) => Ok(Json(
            imgs.into_iter()
                .map(|img| img.imagename)
                .collect::<Vec<String>>(),
        )),
        Err(_) => Err("Invalid request".to_string()),
    }
}

#[post("/newsroom/upload/post", data = "<new_post>", rank = 2)]
pub fn upload_post(conn: DbConn, _admin: Admin, new_post: Json<Post>) -> Status {
    use crate::schema::posts::dsl::*;
    if new_post.id < 0 {
        insert_into(posts)
            .values((
                title.eq(&new_post.title),
                images.eq(&new_post.images),
                content.eq(&new_post.content),
                posttime.eq(new_post.posttime),
            ))
            .execute(&conn.0)
            .unwrap();
    } else {
        println!("in the else");
        let target = posts.filter(id.eq(new_post.id));
        println!("{:?}", target);
        diesel::update(target)
            .set((
                title.eq(&new_post.title),
                images.eq(&new_post.images),
                content.eq(&new_post.content),
                posttime.eq(&new_post.posttime),
            ))
            .execute(&conn.0)
            .unwrap();
    }
    Status::Accepted
}

#[get("/newsroom/thumbnail/<image>", rank = 2)]
pub fn thumbnails(conn: DbConn, image: String) -> Content<Stream<Cursor<Vec<u8>>>> {
    use crate::schema::images::dsl::*;
    let mut buffer: Vec<u8> = Vec::new();
    let mut cursor = Cursor::new(buffer);

    match images.filter(imagename.eq(image)).load::<Image>(&conn.0) {
        Ok(imgs) => {
            match imgs.first() {
                Some(_) => {
                    image::load_from_memory(
                        &imgs[0].thumbnail, // either this is incorrect or my encoding into the DB is incorrect
                    )
                    .unwrap()
                    .write_to(
                        &mut cursor,
                        if let Ok(format) = ImageFormat::from_path(&imgs[0].imagename) {
                            image::ImageOutputFormat::from(format)
                        } else {
                            image::ImageOutputFormat::Unsupported(
                                "Invalid image in database".to_string(),
                            )
                        },
                    )
                    .expect("");
                }
                None => (),
            }
            cursor.set_position(0);
            Content(ContentType::Binary, Stream::from(cursor))
        }
        Err(_) => {
            cursor.set_position(0);
            Content(ContentType::Binary, Stream::from(cursor))
        }
    }
}
#[get("/newsroom/images/<image>", rank = 1)]
pub fn images(conn: DbConn, image: String) -> Content<Stream<Cursor<Vec<u8>>>> {
    use crate::schema::images::dsl::*;
    let mut buffer: Vec<u8> = Vec::new();
    let mut cursor = Cursor::new(buffer);

    match images.filter(imagename.eq(image)).load::<Image>(&conn.0) {
        Ok(imgs) => {
            match imgs.first() {
                Some(_) => {
                    image::load_from_memory(&imgs[0].main)
                        .unwrap()
                        .write_to(
                            &mut cursor,
                            if let Ok(format) = ImageFormat::from_path(&imgs[0].imagename) {
                                image::ImageOutputFormat::from(format)
                            } else {
                                image::ImageOutputFormat::Unsupported(
                                    "Invalid image in database".to_string(),
                                )
                            },
                        )
                        .expect("");
                }
                None => (),
            }
            cursor.set_position(0);
            Content(ContentType::Binary, Stream::from(cursor))
        }
        Err(_) => {
            cursor.set_position(0);
            Content(ContentType::Binary, Stream::from(cursor))
        }
    }
}

fn main() {
    rocket::ignite()
        .mount(
            "/",
            routes![
                files,
                index,
                posts,
                images,
                thumbnails,
                get_images,
                upload_image,
                upload_post
            ],
        )
        .attach(DbConn::fairing())
        .attach(cors::CorsFairing)
        .launch();
}
