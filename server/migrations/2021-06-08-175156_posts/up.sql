-- Your SQL goes here
CREATE TABLE posts (
    id SERIAL NOT NULL PRIMARY KEY,
    title TEXT NOT NULL,
    images TEXT[] NOT NULL,
    content TEXT NOT NULL,
    posttime TIMESTAMPTZ NOT NULL
)