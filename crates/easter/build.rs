#[cfg(feature = "nightly")]
fn main() {}

#[cfg(not(feature = "nightly"))]
#[macro_use]
extern crate derive_context;

#[cfg(not(feature = "nightly"))]
use derive_context::{Context, Registry, Expanded, parse_items};

#[cfg(not(feature = "nightly"))]
fn main() {
    use std::env;
    use std::path::{PathBuf, Path as FilePath};

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let out_path = FilePath::new(&out_dir);

    let src_path = {
        let mut buf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        buf.push("src");
        buf
    };

    let context = Context::new();

    let registry = {
        let mut registry = Registry::new();

        macro_rules! register_tracking_derive {
            ($for_trait:ident, $method:ident) => {
                registry.add_derive(stringify!($for_trait), |input| {
                    let tokens = context.$method(&input);
                    Ok(Expanded {
                        new_items: parse_items(&tokens.to_string())?,
                        original: Some(input),
                    })
                });
            }
        }

        register_tracking_derives!();

        registry
    };

    for entry in src_path.read_dir().unwrap() {
        let path = entry.unwrap().path();
        let file_name = path.file_name().unwrap();
        if file_name != "lib.rs" {
            let dest = out_path.join(file_name);
            registry.expand_file(&path, &dest).unwrap();
        }
    }
}