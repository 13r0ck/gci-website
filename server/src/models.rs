/* Import macros and others */
use crate::schema::*;
use chrono::NaiveDateTime;

/* For beeing able to serialize */
//use serde::Serialize;

#[derive(Debug, Queryable, Serialize)]
pub struct Post {
    pub id: i32, 
    pub title: String,
    pub images: Vec<String>,
    pub content: String,
    pub posttime: NaiveDateTime,
}

#[derive(Debug, Insertable)]
#[table_name="posts"]
pub struct NewPost<'x> {
    pub title: &'x str,
    pub images: Vec<String>,
    pub content: &'x str,
    pub posttime: NaiveDateTime,
}

#[derive(Debug, Queryable, Serialize)]
pub struct Image {
    pub imagename: String,
    pub caption: Option<String>,
    pub main: Vec<u8>,
    pub thumbnail: Vec<u8>,
    pub showthumbnail: bool,
}

#[derive(Debug, Insertable, AsChangeset)]
#[table_name="images"]
pub struct NewImage {
    pub imagename: String,
    pub main: Vec<u8>,
    pub caption: Option<String>,
    pub thumbnail: Vec<u8>,
    pub showthumbnail: bool,
}