-- Your SQL goes here
CREATE TABLE images (
    imageName TEXT NOT NULL PRIMARY KEY,
    caption TEXT,
    main BYTEA NOT NULL,
    thumbnail BYTEA NOT NULL,
    showThumbnail BOOLEAN NOT NULL
)