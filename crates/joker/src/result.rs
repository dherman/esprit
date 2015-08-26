use std::result;
use error::Error;

pub type Result<T> = result::Result<T, Error>;
