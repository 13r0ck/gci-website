use rocket::Outcome;
use rocket::http::Status;
use rocket::request::{self, Request, FromRequest};
use rocket::response::{NamedFile, Responder, Response};
use rocket::response;
use google_signin;

const GOOGLE_CLIENT_ID: &str = "904165140417-upr6ca4hqgharv344ocq3dbrh7c3ns7k.apps.googleusercontent.com";
const ADMINS: &str = env!("ADMINS");


/// Returns sub identifier if `key` is a valid API key string.
fn is_valid(token: &str) -> Option<String> {
	let mut client = google_signin::Client::new();
	client.audiences.push(GOOGLE_CLIENT_ID.to_string()); // required
	//client.hosted_domains.push(YOUR_HOSTED_DOMAIN);
	let id_info = client.get_slow_unverified(token).expect("Expected token to exist");
	if id_info.verify(&client).is_ok() && ADMINS.split(",").into_iter().any(|admin_sub| admin_sub == id_info.sub ){
		println!("{}-(sub: {}) accessed admin only request", id_info.email.unwrap_or("No email provided".to_string()), id_info.sub );
		Some(id_info.sub)
	} else {
		println!("{}-(sub: {}) failed to access admin only request", id_info.email.unwrap_or("No email provided".to_string()), id_info.sub );
		None
	}
}

#[derive(Debug)]
pub enum IdTokenError {
    Missing,
    Invalid,
}

#[derive(Debug)]
pub struct Admin(String);

impl<'a, 'r> FromRequest<'a, 'r> for Admin {
    type Error = IdTokenError;

    fn from_request(request: &'a Request<'r>) -> request::Outcome<Self, Self::Error> {
        if let Some(id_token) = request.headers().get_one("idToken") {
		if let Some(sub) = is_valid(id_token) {
			Outcome::Success(Admin(sub))
		} else {Outcome::Failure((Status::BadRequest, IdTokenError::Invalid))}
	} else {Outcome::Failure((Status::BadRequest, IdTokenError::Missing))}
    }
}

pub struct CachedFile(pub NamedFile,pub usize);

impl<'r> Responder<'r> for CachedFile {
    fn respond_to(self, req: &Request) -> response::Result<'r> {
        Response::build_from(self.0.respond_to(req)?)
            .raw_header("Cache-control", format!("max-age={}", self.1))
            .ok()
    }
}