use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

use quote::{ format_ident, quote };

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let _ = input;

    unimplemented!()
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
