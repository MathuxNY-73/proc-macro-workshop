use proc_macro::TokenStream;

use std::collections::HashSet;
use syn::{parse_macro_input, parse_quote, spanned::Spanned, DeriveInput, Token};
use quote::quote;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    // eprintln!("{:#?}", input);
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
    fields: &'a syn::punctuated::Punctuated<syn::Field, Token![,]>
) -> impl Iterator<Item = &'a syn::Type> {
    let not_inner_ty = move |ps: &syn::PathSegment| match ps {
        syn::PathSegment {
            ident,
            arguments: syn::PathArguments::AngleBracketed(_) | syn::PathArguments::None,
        } if ident != ty => true,
        _ => false
    };

    fields.iter().filter_map(move |f| match f.ty {
        ref typ @ syn::Type::Path(syn::TypePath {
            path: syn::Path {
                ref segments,
                ..
            },
            ..
        }) if segments.last().map_or(false, not_inner_ty) => Some(typ),
        _ => None,
    })
}

fn get_simple_generic_type_ident(ty: &syn::Type) -> HashSet<&syn::Ident> {
    let segment = match ty {
        syn::Type::Path(
            syn::TypePath {
                path: syn::Path {
                    ref segments,
                    ..
                },
                ..
            }) => segments.last(),
        _ => None,
    };
    match segment {
        Some(syn::PathSegment {
            arguments: syn::PathArguments::AngleBracketed(
                syn::AngleBracketedGenericArguments { 
                    args,
                    .. }),
                ..
            }) => args.iter().filter_map(|arg| match arg {
                syn::GenericArgument::Type(typ) => Some(get_simple_generic_type_ident(typ)),
                _ => None,
            })
            .flatten()
            .collect::<HashSet<_>>(),
        Some(syn::PathSegment {
            ident,
            arguments: syn::PathArguments::None,
        }) => HashSet::from([ident]),
        _ => HashSet::<&syn::Ident>::new(),
    }
}

fn get_associated_generic_type<'a>(
        ty: &'a syn::Type,
        generics: &HashSet<&syn::Ident>) -> HashSet<&'a syn::Type> {
    let path_segment_is_generic = |ps: &syn::PathSegment| match ps {
        syn::PathSegment {
            ident,
            arguments: syn::PathArguments::None,
        } if generics.contains(ident) => true,
        _ => false,
    };

    let syn::Type::Path(
        syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) = ty else {
            return HashSet::new();
        };

    if let Some(syn::PathSegment {
        arguments: syn::PathArguments::AngleBracketed(
            syn::AngleBracketedGenericArguments {
                args,
                ..
            }
        ),
        ..
    }) = segments.last() {
        args.iter().filter_map(|arg| match arg {
            syn::GenericArgument::Type(ty) => Some(get_associated_generic_type(ty, generics)),
            _ => None,
        })
        .flatten()
        .collect::<HashSet<_>>()
    } else if segments.len() > 1 && segments.first().map_or(false, path_segment_is_generic) {
        HashSet::from([ty])
    } else {
        HashSet::new()
    }
}

fn add_trait_bounds(
        fields: &syn::punctuated::Punctuated<syn::Field, Token![,]>,
        mut generics: syn::Generics) -> syn::Generics {
    // eprintln!("fields {:#?}", fields);
    let gen_typs = get_type_params("PhantomData", fields).collect::<Vec<_>>();
    // eprintln!("{:#?}", gen_typs);
    let simple_typs = gen_typs.clone().into_iter().map(|ty| get_simple_generic_type_ident(ty)).flatten().collect::<HashSet<_>>();
    let generic_types = generics.type_params().map(|tp| &tp.ident).collect::<HashSet<_>>();
    let associated_types = gen_typs.into_iter().map(|ty|
        get_associated_generic_type(ty, &generic_types)).flatten().collect::<HashSet<_>>();

    for param in generics.type_params_mut() {
        if simple_typs.contains(&param.ident) {
            param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }

    let mut where_clause_predicates = syn::punctuated::Punctuated::<syn::WherePredicate, Token![,]>::new();
    for assoc_typ in associated_types {
        let mut bounds = syn::punctuated::Punctuated::<syn::TypeParamBound, Token![+]>::new();
        bounds.push(parse_quote!(std::fmt::Debug));
        where_clause_predicates.push(syn::WherePredicate::Type(syn::PredicateType {
            lifetimes: None,
            bounded_ty: assoc_typ.clone(),
            colon_token: Token![:](generics.span()),
            bounds,
        }));
    }
    if let Some(ref mut where_clause) = generics.where_clause {
        where_clause.predicates.extend(where_clause_predicates.into_iter());
    } else {
        generics.where_clause = Some(syn::WhereClause {
            where_token: Token![where](generics.span()),
            predicates: where_clause_predicates
        });
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
