use std::result;
use error::Error;

pub type Result<T> = result::Result<T, Error>;

pub trait Map<T, U> {
    fn map<F: Fn(T) -> Result<U>>(self, F) -> Result<Vec<U>>;
}

impl<T, U> Map<T, U> for Vec<T> {
    fn map<F: Fn(T) -> Result<U>>(self, f: F) -> Result<Vec<U>> {
        let mut list = Vec::with_capacity(self.len());
        for data in self {
            list.push(f(data)?);
        }
        Ok(list)
    }
}
