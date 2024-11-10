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

    match validate(&input) {
        Ok(()) => input.to_token_stream(),
        Err(e) => {
            let err = e.into_compile_error();
            let output = input.to_token_stream();
            quote! { #output #err }
        },
    }.into()
}

fn validate(input: &Item) -> syn::Result<()> {
    let Item::Enum(enum_item @ syn::ItemEnum { .. }) = input else {
        return Err(syn::Error::new(proc_macro2::Span::call_site(), "expected enum or match expression"));
    };

    validate_ident_order(get_variant_ident(enum_item).into_iter())?;
    Ok(())
}

fn get_variant_ident(item: &syn::ItemEnum) -> Vec<&syn::Ident> {
    item.variants.iter().map(|variant| &variant.ident).collect::<Vec<_>>()
}

fn validate_ident_order<'a>(idents: impl IntoIterator<Item = &'a syn::Ident>) -> syn::Result<()> {
    let mut variants = idents.into_iter().enumerate().map(|(i, variant)| (variant, i)).collect::<Vec<_>>();
    variants.sort();

    let mut error: Option<syn::Error> = None;
    let mut out_of_order_cnt = 0;
    for (i, (ident, orig_idx)) in variants.iter().enumerate() {
        if *orig_idx + out_of_order_cnt > i && i < variants.len() - 1 {
            let err = syn::Error::new(
                ident.span(), format!("{} should sort before {}", ident, variants[i + 1].0));
            error = error.map(|mut e| { e.combine(err.clone()); e } ).or(Some(err));
            out_of_order_cnt += 1;
        } else if *orig_idx + out_of_order_cnt <= i && out_of_order_cnt > 0 {
            out_of_order_cnt -= 1;
        }
    }

    if let Some(e) = error {
        Err(e)
    } else {
        Ok(())
    }
}
