-- Your SQL goes here
CREATE TABLE images (
    id SERIAL NOT NULL PRIMARY KEY,
    main BYTEA NOT NULL,
    thumbnail BYTEA NOT NULL,
    showThumbnail BOOLEAN NOT NULL
)