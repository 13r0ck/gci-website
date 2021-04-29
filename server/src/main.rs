#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use] extern crate rocket;
use rocket_contrib::serve::StaticFiles;
use rocket::response::Redirect;

#[get("/")]
fn index() -> Redirect {
    Redirect::to("/index.html")
}


fn main() {
    rocket::ignite()
        .mount("/", StaticFiles::from("/server/public"))
        .launch();
}
