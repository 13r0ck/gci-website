#![feature(proc_macro_hygiene, decl_macro)]
use rocket::Request;
use rocket::Response;
use rocket::response::Responder;
use self::diesel::prelude::*;

#[macro_use]
extern crate rocket;
use rocket::response;
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
use models::{Post, Image};
use rocket::response::Content;
use rocket::http::ContentType;
use rocket::response::Stream;
use std::io::Cursor;
use image::ImageFormat;

pub struct CachedFile(NamedFile, usize);

impl<'r> Responder<'r> for CachedFile {
    fn respond_to(self, req: &Request) -> response::Result<'r> {
        Response::build_from(self.0.respond_to(req)?)
            .raw_header("Cache-control", format!("max-age={}", self.1))
            .ok()
    }
}

#[database("newsroom")]
pub struct DbConn(diesel::PgConnection);

#[get("/")]
fn index() -> Option<CachedFile> {
    NamedFile::open(Path::new("server/public/index.html")).ok().map(|n| CachedFile(n, 0))
}

#[get("/<file>", rank = 10)]
fn files(file: String) -> Option<CachedFile> {
    NamedFile::open(Path::new("server/public/").join(file))
        .ok().map(|n| CachedFile(n, 31536000)) //  1 year (24*60*60*365)
        .or_else(|| NamedFile::open(Path::new("server/public/index.html")).ok().map(|n| CachedFile(n, 0)))
}

#[post("/newsroom/posts?<range>")]
pub fn posts(conn: DbConn, range: i64) -> Result<Json<Vec<Post>>, String> {
    use crate::schema::posts::dsl::*;

    posts.order(posttime.desc()).limit(range.min(10)).load(&conn.0).map_err(|err| -> String {
        println!("Error querying page views: {:?}", err);
        "Error querying page views from the database".into()
    }).map(Json)
}

#[get("/newsroom/images/<image>")]
pub fn images(conn: DbConn, image: String) -> Content<Stream<Cursor<Vec<u8>>>> {
    use crate::schema::images::dsl::*;
    let mut buffer: Vec<u8> = Vec::new();
    let mut cursor = Cursor::new(buffer);

    match images.filter(imagename.eq(image)).load::<Image>(&conn.0) {
        Ok(imgs) => {
            //println!("{:?}", imgs[0].main);
            //println!("{:?}", image::load_from_memory(&imgs[0].main));
            println!("{:?}", image::load_from_memory(
                &base64::decode(&imgs[0].main).unwrap()
                ).unwrap().write_to(&mut cursor, image::ImageOutputFormat::from(ImageFormat::Jpeg)).expect("")
            );
            cursor.set_position(0);
            Content(ContentType::Binary, Stream::from(cursor))
            /*
            NamedFile::open(Path::new("../public/img/logo.jpg"))
                .ok().map(|n| CachedFile(n, 0)).or_else(|| None)
                */
        }
        Err(_) => {
            cursor.set_position(0);
            Content(ContentType::Binary, Stream::from(cursor))
        }
    }
}

fn main() {
    rocket::ignite()
        .mount("/", routes![files, index, posts, images])
        .attach(DbConn::fairing())
        .attach(cors::CorsFairing)
        .launch();
}
