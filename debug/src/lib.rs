use proc_macro::TokenStream;

use syn::{parse_macro_input, parse_quote, DeriveInput, Token};
use quote::quote;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    eprintln!("{:#?}", input);
    let name = &input.ident;
    let syn::Data::Struct(
        syn::DataStruct {
            fields: syn::Fields::Named(
                syn::FieldsNamed{ named: fields, .. }), ..} ) = &input.data else {
        unreachable!()
    };

    let generics = add_trait_bounds(fields, input.generics);

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let fields_debug = fields.iter().map(|f| {
        let name = &f.ident;
        let dbg_fmt = get_debug_format(f);

        if let Some(dbg_fmt) = dbg_fmt {
            quote! {
                .field(stringify!(#name), &std::format_args!(#dbg_fmt, &self.#name))
            }
        } else {
            quote! {
                .field(stringify!(#name), &self.#name)
            }
        }
    });
    let tokens = quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#name))
                #(#fields_debug)*
                .finish()
            }
        }
    };
    tokens.into()
}

fn get_type_params<'a>(
    ty: &'a str,
    fields: &'a syn::punctuated::Punctuated<syn::Field, Token![,]>,
    generics: &'a mut syn::Generics) -> impl Iterator<Item = &'a mut syn::TypeParam> {
        generics.type_params_mut().filter(move |g| {
            fields.iter().filter_map(|f| match f.ty {
                syn::Type::Path(syn::TypePath {
                    path: syn::Path {
                        ref segments,
                        ..
                    },
                    ..
                }) => Some(segments),
                _ => None,
            })
            .filter_map(|segments| match segments.last() {
                Some(syn::PathSegment {
                    ref ident,
                    arguments: syn::PathArguments::AngleBracketed(
                        syn::AngleBracketedGenericArguments {
                            ref args,
                            ..
                        })
                    }) if ident == ty => Some(args),
                    _ => None,
            })
            .flatten()
            .filter(|arg| match arg {
                    syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                        path: syn::Path {
                            ref segments,
                            ..
                        },
                        ..
                    })) if segments.len() == 1 && segments.first().map_or(false, |f| f.ident == g.ident) => true,
                    _ => false,
            })
            .count() == 0
        })
}

fn add_trait_bounds(
        fields: &syn::punctuated::Punctuated<syn::Field, Token![,]>,
        mut generics: syn::Generics) -> syn::Generics {
    for ty in get_type_params("PhantomData", fields, &mut generics) {
        ty.bounds.push(parse_quote!(std::fmt::Debug));
    }
    generics
}

fn get_debug_format(field: &syn::Field) -> Option<String> {
    let mut fmtstrs = field.attrs.iter().filter(|attr| match attr.meta {
        syn::Meta::NameValue(
            syn::MetaNameValue {
                path: syn::Path {
                    ref segments,
                    ..
                },
                ..
            }) => segments.len() == 1 && segments.last().map_or(false, |s| s.ident == "debug"),
            _ => false
    })
    .map(|attr| match attr.meta {
        syn::Meta::NameValue(
            syn::MetaNameValue {
                value: syn::Expr::Lit(
                    syn::ExprLit {
                        lit: syn::Lit::Str(ref fmtstr),
                        ..
                    }
                ),
                ..
            }
        ) => fmtstr.value(),
        _ => panic!("Expected string value."),
    })
    .collect::<Vec<_>>();

    if fmtstrs.len() > 1 {
        panic!("Only 1 format string should be provided.");
    }
    fmtstrs.pop()
}
