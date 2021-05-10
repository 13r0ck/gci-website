#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use]
extern crate rocket;
use rocket::response::NamedFile;
use std::path::{Path, PathBuf};

#[get("/")]
fn index() -> Option<NamedFile> {
    NamedFile::open(Path::new("server/public/index.html")).ok()
}

#[get("/<file..>", rank = 10)]
fn files(file: PathBuf) -> Option<NamedFile> {
    NamedFile::open(Path::new("server/public/").join(file))
        .ok()
        .or_else(|| NamedFile::open(Path::new("server/public/index.html")).ok())
}

fn main() {
    rocket::ignite().mount("/", routes![files, index]).launch();
}
