table! {
    images (id) {
        id -> Int4,
        main -> Bytea,
        thumbnail -> Bytea,
        showthumbnail -> Bool,
    }
}

table! {
    posts (id) {
        id -> Int4,
        title -> Text,
        images -> Array<Int4>,
        content -> Text,
        posttime -> Date,
    }
}

allow_tables_to_appear_in_same_query!(
    images,
    posts,
);
