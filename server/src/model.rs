/* Import macros and others */
use crate::schema::*;

/* For beeing able to serialize */
use serde::Serialize;

#[derive(Debug, Queryable, Serialize)]
pub struct Post {
    pub id: i32, 
    pub title: String,
    pub images: Vec<i32>,
    pub content: String,
    pub date: chrono::DateTime,
}

#[derive(Debug, Insertable, AsChangeset)]
#[table_name="posts"]
pub struct NewPost<'x> {
    pub title: &'x str,
    pub images: Vec<i32>,
    pub content: &'x str,
    pub date: chrono::DateTime,
}

#[derive(Debug, Queryable, Serialize)]
pub struct Image {
    pub id: i32,
    pub title: String,
    pub data: Vec<u8>
}

#[derive(Debug, Insertable, AsChangeset)]
#[table_name="images"]
pub struct NewImage<'x> {
    pub data: Vec<u8>,
    pub thumbnail: Vec<u8>
}