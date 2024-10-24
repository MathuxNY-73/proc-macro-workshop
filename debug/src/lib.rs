use proc_macro::TokenStream;

use std::collections::HashSet;
use syn::{parse_macro_input, parse_quote, spanned::Spanned, DeriveInput, Token};
use quote::quote;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let (bounds, errors): (Vec<_>, Vec<_>) = input
        .attrs
        .iter()
        .map(get_bound)
        .filter(|bnd| bnd.as_ref().map_or(true, Option::is_some))
        .map(|bnd| bnd.map(|b| syn::parse_str::<syn::WherePredicate>(&b.unwrap().value())))
        .flatten()
        .partition(Result::is_ok);

    if !errors.is_empty() {
        let mut errors = errors.into_iter().map(syn::Result::unwrap_err);
        let mut first = errors.next().unwrap();  // Safe since there is at least one error.
        for err in errors {
            first.combine(err);
        }
        return first.into_compile_error().into();
    }
    
    let bounds = bounds.into_iter().map(syn::Result::unwrap).collect::<Vec<_>>();

    let name = &input.ident;
    let syn::Data::Struct(
        syn::DataStruct {
            fields: syn::Fields::Named(
                syn::FieldsNamed{ named: fields, .. }), ..} ) = &input.data else {
        unreachable!()
    };

    // TODO(MathuxNY-73): Refactor this part.
    let generics = if bounds.is_empty() {
        add_trait_bounds(fields, input.generics)
    } else {
        input.generics
    };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let where_clause = if let Some(where_clause) = where_clause {
        let mut where_clause = where_clause.clone();
        where_clause.predicates.extend(bounds.into_iter());
        where_clause
    } else {
        let mut where_clause = syn::WhereClause {
            predicates: Default::default(),
            where_token: Default::default(),
        };
        where_clause.predicates.extend(bounds.into_iter());
        where_clause
    };

    let fields_debug = fields.iter().map(|f| {
        let name = &f.ident;
        match get_debug_format(f) {
            Ok(Some(dbg_fmt)) => quote! {
                .field(stringify!(#name), &std::format_args!(#dbg_fmt, &self.#name))
            },
            Ok(_) => quote! {
                .field(stringify!(#name), &self.#name)
            },
            Err(err) => err.into_compile_error(),
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

fn get_debug_format(field: &syn::Field) -> syn::Result<Option<String>> {
    let mut fmtstrs = field.attrs.iter()
        .filter(|attr| attr.path().is_ident("debug"))
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
            ) => Ok(fmtstr.value()),
            _ => Err(syn::Error::new(attr.meta.span(), "expected `debug = \"...\"`")),
        })
        .collect::<syn::Result<Vec<_>>>()?;

    if fmtstrs.len() > 1 {
        return Err(syn::Error::new(field.span(), "expected to have only one `debug` argument."));
    }
    Ok(fmtstrs.pop())
}

fn get_bound(attr: &syn::Attribute) -> syn::Result<Option<syn::LitStr>> {
    if !attr.path().is_ident("debug") {
        return Ok(None);
    }

    match attr.parse_args_with(
        syn::punctuated::Punctuated::<syn::MetaNameValue, Token![,]>::parse_terminated) {
            Ok(mnvs) => Ok(mnvs.into_iter().filter_map(|mnv| {
                eprintln!("{:#?}", mnv);
                if mnv.path.is_ident("bound") {
                    Some(mnv.value.clone())
                } else {
                    None
                }
            })
            .filter_map(|expr| match expr {
                syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(s),
                    ..
                }) => Some(s),
                _ => None,
            })
            .next()),
            Err(err) => Err(syn::Error::new(attr.span(), format!("could not parse attribute arguments: {err}"))),
    }


}
