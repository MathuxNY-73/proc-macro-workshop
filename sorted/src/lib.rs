use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

use quote::quote;
use syn::{parse_macro_input, Item};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = args;
    let input = parse_macro_input!(input as Item);
    eprintln!("args: {:#?}", args);
    eprintln!("input: {:#?}", input);

    match expand(input) {
        Ok(ts) => ts,
        Err(e) => e.into_compile_error(),
    }.into()
}

fn expand(input: Item) -> syn::Result<TokenStream2> {
    let Item::Enum(enum_item @ syn::ItemEnum { .. }) = input else {
        return Err(syn::Error::new(proc_macro2::Span::call_site(), "expected enum or match expression"));
    };

    Ok(quote! {})
}
