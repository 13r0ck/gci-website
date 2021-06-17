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
pub mod models;
pub mod schema;
pub mod cors;
pub mod types;
use types::{CachedFile, Admin};
use models::{Post, Image};
use rocket::response::Content;
use rocket::http::ContentType;
use rocket::response::Stream;
use std::io::Cursor;
use image::ImageFormat;




#[database("newsroom")]
pub struct DbConn(diesel::PgConnection);

#[get("/")]
fn index() -> Option<CachedFile> {
    NamedFile::open(Path::new("server/public/index.html")).ok().map(|n| CachedFile(n, 0))
}

#[get("/<file..>", rank = 10)]
fn files(file: PathBuf) -> Option<CachedFile> {
    NamedFile::open(Path::new("server/public/").join(file))
        .ok().map(|n| CachedFile(n, 31536000)) //  1 year (24*60*60*365)
        .or_else(|| NamedFile::open(Path::new("server/public/index.html")).ok().map(|n| CachedFile(n, 0)))
}

#[post("/newsroom/posts?<i>&<range>")]
pub fn posts(conn: DbConn, i : i64, range: i64) -> Result<Json<Vec<Post>>, String> {
    use crate::schema::posts::dsl::*;

    posts.order(posttime.desc()).offset(i).limit(range.min(3)).load(&conn.0).map_err(|_err| -> String {
        "Error querying page views from the database".into()
    }).map(Json)
}

#[post("/newsroom/getimages")]
pub fn get_images(conn: DbConn, admin: Admin) -> Result<Json<Vec<String>>, String> {
    use crate::schema::images::dsl::*;
    match images.load::<Image>(&conn.0) {
        Ok(imgs) => Ok(Json(imgs.into_iter().map(|img| img.imagename).collect::<Vec<String>>())),
        Err(_) => Err("Invalid request".to_string()),
    }
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
                        &base64::decode(&imgs[0].thumbnail).unwrap()
                        ).unwrap().write_to(&mut cursor, if let Ok(format) = ImageFormat::from_path(&imgs[0].imagename) {
                            image::ImageOutputFormat::from(format)
                        } else {image::ImageOutputFormat::Unsupported("Invalid image in database".to_string())}).expect("");

                    }
                None => ()
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
                    image::load_from_memory(
                        &base64::decode(&imgs[0].main).unwrap()
                        ).unwrap().write_to(&mut cursor, if let Ok(format) = ImageFormat::from_path(&imgs[0].imagename) {
                            image::ImageOutputFormat::from(format)
                        } else {image::ImageOutputFormat::Unsupported("Invalid image in database".to_string())}).expect("");

                    }
                None => ()
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
        .mount("/", routes![files, index, posts, images, thumbnails, get_images])
        .attach(DbConn::fairing())
        .attach(cors::CorsFairing)
        .launch();
}
