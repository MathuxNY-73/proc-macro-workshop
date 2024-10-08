use proc_macro::TokenStream;

use syn::{parse_macro_input, DeriveInput, Ident, Token};
use quote::quote;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let builder_name = format!("{}Builder", name);
    let builder_ident = Ident::new(&builder_name, name.span());

    let fields = if let syn::Data::Struct(
        syn::DataStruct {
            fields: syn::Fields::Named(
                syn::FieldsNamed {ref named, .. }), ..}) = input.data {
        named
    } else {
        unreachable!()
    };

    let optionized = fields.iter().map(|f| {
        // let mut generic_args = syn::punctuated::Punctuated::new();
        // generic_args.push(syn::GenericArgument::Type(f.ty.clone()));
        // let mut punctuated = syn::punctuated::Punctuated::new();
        // punctuated.push(syn::PathSegment {
        //     ident: syn::Ident::new("std", Span::call_site()),
        //     arguments: syn::PathArguments::None });
        // punctuated.push(syn::PathSegment {
        //     ident: syn::Ident::new("option", Span::call_site()),
        //     arguments: syn::PathArguments::None });
        // punctuated.push(
        //     syn::PathSegment {
        //         ident: syn::Ident::new("Option", Span::call_site()),
        //         arguments: syn::PathArguments::AngleBracketed(
        //             syn::AngleBracketedGenericArguments {
        //                 colon2_token: None,
        //                 lt_token: syn::token::Lt::default(),
        //                 args: generic_args,
        //                 gt_token: syn::token::Gt::default(),
        //             })});
        // let ty = syn::Type::Path(
        //     syn::TypePath {
        //         qself: None,
        //         path: syn::Path {
        //             leading_colon: None,
        //             segments: punctuated,
        //         }
        //     }
        // );
        // syn::Field {
        //     attrs: Vec::new(),
        //     vis: syn::Visibility::Inherited,
        //     ident: f_ident.clone(),
        //     colon_token: f.colon_token,
        //     ty,
        //     mutability: f.mutability.clone(),
        // }
        let f_ident = f.ident.clone();
        let ty = f.ty.clone();
        if ty_inner_type("Option", &ty).is_some()
            || ty_inner_type("Vec", &ty).is_some() {
            quote! {
                #f_ident: #ty
            }
        } else {
            quote! {
                #f_ident: std::option::Option<#ty>
            }
        }
    });

    let extend_methods =
        fields.iter().map(|f| extend_method(f))
            .filter_map(|em| match em {
                Some((e, m)) if !e => Some(m),
                _ => None,
            });

    // let extend_methods =
    // fields.iter()
    //     .map(|f| &f.attrs)
    //     .filter(|attrs| !attrs.is_empty())
    //     .map(|attrs| {
    //         attrs.iter().filter_map(|a| match &a.meta {
    //             syn::Meta::List(
    //                 syn::MetaList { 
    //                     path: syn::Path { segments: segs, .. },
    //                     tokens: tts, ..}) => {

    //                 if segs.first().map_or(false, |ps| ps.ident == "builder") {
    //                     let mut fns = syn::punctuated::Punctuated::<syn::MetaNameValue, Token![,]>::parse_terminated
    //                         .parse2(tts.clone())
    //                         .unwrap()
    //                         .iter()
    //                         .filter(|mnv| mnv.path.segments.last().map_or(false, |l| l.ident == "each"))
    //                         .map(|mnv| match &mnv.value {
    //                             syn::Expr::Lit(syn::ExprLit{
    //                                     lit: syn::Lit::Str(s), .. } ) => s,
    //                             _ => panic!("Expected a string literal got {:#?}", mnv.value),
    //                         })
    //                         .map(|nam| {
    //                             let ident = Ident::new(&nam.value(), Span::call_site());
    //                             quote! {
    //                                 fn #ident(&mut self, arg: String) -> &mut Self {
    //                                     self.#ident.push_back();
    //                                     self
    //                                 }
    //                             }
    //                         })
    //                         .collect::<Vec<_>>();
    //                     eprintln!("{:#?}", fns);
    //                     if fns.len() > 1 {
    //                         panic!("Should have only 1 `each` attribute");
    //                     } else {
    //                         fns.pop()
    //                     }
    //                 } else {
    //                     None
    //                 }
    //             },
    //             _ => None
    //         })
    //     })
    //     .flatten();
    let builder_methods = fields.iter().map(|f| {
        let f_ident = f.ident.clone();
        if let Some(ty) = ty_inner_type("Option", &f.ty) {
            quote! {
                fn #f_ident(&mut self, #f_ident: #ty) -> &mut Self {
                    self.#f_ident = Some(#f_ident);
                    self
                }
            }
        } else if ty_inner_type("Vec", &f.ty).is_some() {
            let ty = &f.ty;
            quote! {
                fn #f_ident(&mut self, #f_ident: #ty) -> &mut Self {
                    self.#f_ident = #f_ident;
                    self
                }
            }
        } else {
            let ty = &f.ty;
            quote! {
                fn #f_ident(&mut self, #f_ident: #ty) -> &mut Self {
                    self.#f_ident = Some(#f_ident);
                    self
                }
            }
        }
    });

    let builder_fields = fields.iter().map(|f| {
        let f_ident = f.ident.clone();

        if ty_inner_type("Option", &f.ty).is_some()
            || ty_inner_type("Vec", &f.ty).is_some() {
            quote! {
                #f_ident: self.#f_ident.clone()
            }
        } else {
            quote! {
                #f_ident: self.#f_ident.clone().ok_or(concat!(stringify!(#f_ident), " is not set."))?
            }
        }
    });

    let builder_fields_default = fields.iter().map(|f| {
        let f_ident = f.ident.clone();
        if ty_inner_type("Vec", &f.ty).is_some() {
            quote! {
                #f_ident: vec![]
            }
        } else {
            quote! {
                #f_ident: None
            }
        }
    });

    let tokens = quote! {
        struct #builder_ident {
            #(#optionized,)*
        }
        impl #builder_ident {
            #(#builder_methods)*
            #(#extend_methods)*

            fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#builder_fields,)*
                })
            }
        }

        impl #name {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_fields_default,)*
                }
            }
        }
    };
    tokens.into()
}

fn ty_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(tp) = ty {
        if tp.path.segments.len() != 1 || !tp.path.segments.last().map_or(false, |p| p.ident == wrapper) {
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner_ty) = tp.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }
            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

fn builder_of(f: &syn::Field) -> Option<&syn::Attribute> {
    for attr in &f.attrs {
        if attr.path().is_ident("builder") {
            return Some(attr);
        }
    }
    None
}

fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    let name = f.ident.as_ref().unwrap();
    let builder_of = builder_of(f);

    fn mk_err<T: quote::ToTokens>(t: T) -> Option<(bool, proc_macro2::TokenStream)> {
        Some((false, syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error()))
    }

    let Some(builder_of) = builder_of else { return None; };
    match builder_of.parse_args_with(syn::punctuated::Punctuated::<syn::MetaNameValue, Token![,]>::parse_terminated) {
        Ok(pmnv) => {
            let each_args = pmnv.iter().filter(|mnv|mnv.path.is_ident("each")).collect::<Vec<_>>();
            if each_args.len() != 1 {
                return mk_err(&builder_of.meta);
            } 
            each_args
            .into_iter()
            .map(|mnv| match &mnv.value {
                syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(s), .. }) => s,
                _ => panic!("Expected a string literal got {:#?}", mnv.value),
            })
            .take(1)
            .map(|li| {
                let id = syn::Ident::new(&li.value(), li.span());
                let ty = ty_inner_type("Vec", &f.ty).unwrap();
                (&id == name, quote! {
                    fn #id(&mut self, #id: #ty) -> &mut Self {
                        self.#name.push(#id);
                        self
                    }
                })
            })
            .collect::<Vec<_>>()
            .pop()
        },
        Err(e) => Some((false, e.to_compile_error())),
    }
}
