extern crate proc_macro;
use proc_macro::TokenStream;

#[macro_use]
extern crate derive_context;
use derive_context::{Context, parse_macro_input};

macro_rules! register_tracking_derive {
    ($for_trait:ident, $method:ident) => {
        #[proc_macro_derive($for_trait)]
        pub fn $method(input: TokenStream) -> TokenStream {
            let source = input.to_string();
            let context = Context::new();

            // Parse the string representation into a syntax tree
            let ast = parse_macro_input(&source).unwrap();

            // Build the output, possibly using quasi-quotation
            let expanded = context.$method(&ast);

            // Parse back to a token stream and return it
            expanded.parse().unwrap()
        }
    }
}

register_tracking_derives!();
