use proc_macro::TokenStream;

use quote::quote;
use syn::{Item, parse_macro_input};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = args;
    let input = parse_macro_input!(input as Item);
    eprintln!("args: {:#?}", args);
    eprintln!("input: {:#?}", input);

    let tokens = quote! {};
    tokens.into()
}
