table! {
    images (id) {
        id -> Int4,
        imagename -> Text,
        main -> Bytea,
        thumbnail -> Bytea,
        showthumbnail -> Bool,
    }
}

table! {
    posts (id) {
        id -> Int4,
        title -> Text,
        images -> Array<Text>,
        content -> Text,
        posttime -> Timestamptz,
    }
}

allow_tables_to_appear_in_same_query!(
    images,
    posts,
);
