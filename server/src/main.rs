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
use models::Post;

struct CachedFile(NamedFile, usize);

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

#[get("/<file..>", rank = 10)]
fn files(file: PathBuf) -> Option<CachedFile> {
    NamedFile::open(Path::new("server/public/").join(file))
        .ok().map(|n| CachedFile(n, 31536000)) //  1 year (24*60*60*365)
        .or_else(|| NamedFile::open(Path::new("server/public/index.html")).ok().map(|n| CachedFile(n, 0)))
}

#[get("/newsroom/posts?<first>&<last>")]
pub fn test(conn: DbConn, first: usize, last: usize) -> Result<Json<Vec<Post>>, String> {
    use crate::schema::posts::dsl::*;

    posts.load(&conn.0).map_err(|err| -> String {
        println!("Error querying page views: {:?} {} {}", err, first, last);
        "Error querying page views from the database".into()
    }).map(Json)
}

fn main() {
    rocket::ignite()
        .mount("/", routes![files, index, test])
        .attach(DbConn::fairing())
        .launch();
}
