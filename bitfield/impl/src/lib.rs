use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

use quote::{ format_ident, quote };
use syn::{parse_macro_input, spanned::Spanned, Item};

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Item);

    let tokens = match input {
        syn::Item::Struct(syn::ItemStruct {
            vis,
            fields: syn::Fields::Named(syn::FieldsNamed {
                named,
                ..
            }),
            ident,
            ..
        }) => {
            let fields = named.iter().filter_map(|f| {
                let Some(ident) = f.ident.as_ref() else {
                    return None;
                };
                Some((ident, f))
            });
            let num_fields = fields.clone().count();
            let f_ident = fields.clone().map(|(ident, _)| ident);
            let ty = fields.clone().map(|(_, f)| &f.ty);
            let viz = fields.map(|(_, f)| &f.vis);
            let size = {
                let ty = ty.clone();
                quote! { #( <#ty as ::bitfield::Specifier>::BITS as usize )+* }
            };

            let width = {
                let ty = ty.clone();
                quote! { [#( <#ty as ::bitfield::Specifier>::BITS as usize ),*]}
            };
            let acc = (0..num_fields).into_iter().map(|n| {
                let idx = 0..n;
                quote! { 0 #( + Self::WIDTH[#idx] )* }
            });
            let acc_name = f_ident.clone().map(|id| format_ident!("ACC_{}", id.to_string().to_ascii_uppercase()));
            let getter = f_ident.clone().map(|id| format_ident!("get_{}", id));
            let setter = f_ident.clone().map(|id| format_ident!("set_{}", id));

            quote! {
                #[repr(C)]
                #vis struct #ident {
                    data: [u8; #size >> 3 + ((#size) % 8 != 0) as usize],
                }

                impl #ident {
                    const SIZE: usize = #size >> 3 + ((#size) % 8 != 0) as usize;
                    const WIDTH: [usize; #num_fields] = #width;

                    #vis fn new() -> Self {
                        Self { data: ::std::default::Default::default() }
                    }

                    #(
                        const #acc_name: usize = #acc;

                        #viz fn #getter(&self) -> <#ty as ::bitfield::Specifier>::T {
                            <#ty as ::bitfield::Specifier>::get::<{Self::#acc_name}, {Self::SIZE}>(&self.data)
                        }

                        #viz fn #setter(&mut self, #f_ident: <#ty as ::bitfield::Specifier>::T) {
                            <#ty as ::bitfield::Specifier>::set::<{Self::#acc_name}, {Self::SIZE}>(&mut self.data, #f_ident);
                        }
                    )*
                }
            }
        },
        _ => unimplemented!()
    };

    tokens.into()
}

#[proc_macro]
pub fn gen(_: TokenStream) -> TokenStream {
    fn to_unsigned(bits: usize) -> (proc_macro2::Ident, proc_macro2::Ident) {
        let u = match bits {
            65.. => unreachable!(),
            33.. => 64u8,
            17.. => 32,
            9.. => 16,
            _ => 8,
        };
        (format_ident!("u{}", u), format_ident!("BitsU{}", u))
    }
    let range = 1..=64usize;
    let ident = range.clone().map(|n| format_ident!("B{}", n));
    let (u_ident, bits_u): (Vec<_>, Vec<_>) = range.clone().map(to_unsigned).unzip();

    let tokens = quote! {
        #(
            pub struct #ident;

            impl Specifier for #ident {
                const BITS: usize = #range;
                type T = ::core::primitive::#u_ident;

                fn set<const ACC: usize, const SIZE: usize>(arr: &mut [u8], num: <Self as Specifier>::T) {
                    unimplemented!()
                }

                fn get<const ACC: usize, const SIZE: usize>(arr: &[u8]) -> <Self as Specifier>::T {
                    unimplemented!()
                }
            }
        )*
    };
    tokens.into()
}
