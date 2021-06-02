#![feature(proc_macro_hygiene, decl_macro)]
use rocket::Request;
use rocket::Response;
use rocket::response::Responder;
//use rocket::http::hyper::Request;

#[macro_use]
extern crate rocket;
use rocket::response;
use rocket::response::NamedFile;
use std::path::{Path, PathBuf};

struct CachedFile(NamedFile, usize);

impl<'r> Responder<'r> for CachedFile {
    fn respond_to(self, req: &Request) -> response::Result<'r> {
        Response::build_from(self.0.respond_to(req)?)
            .raw_header("Cache-control", format!("max-age={}", self.1)) //  1 year (24*60*60*365)
            .ok()
    }
}

#[get("/")]
fn index() -> Option<CachedFile> {
    NamedFile::open(Path::new("server/public/index.html")).ok().map(|n| CachedFile(n, 0))
}

#[get("/<file..>", rank = 10)]
fn files(file: PathBuf) -> Option<CachedFile> {
    NamedFile::open(Path::new("server/public/").join(file))
        .ok().map(|n| CachedFile(n, 31536000))
        .or_else(|| NamedFile::open(Path::new("server/public/index.html")).ok().map(|n| CachedFile(n, 0)))
}

fn main() {
    rocket::ignite().mount("/", routes![files, index]).launch();
}
