use proc_macro::TokenStream;

use quote::{ format_ident, quote };
use syn::{parse_macro_input, spanned::Spanned, DeriveInput, Item};

#[proc_macro_attribute]
pub fn bitfield(_args: TokenStream, input: TokenStream) -> TokenStream {
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

            let start_ty = ty.clone();
            let start =
                (0..num_fields).into_iter().map(|n| {
                    let ty = start_ty.clone().take(n);
                    quote! { 0 #( + <#ty as ::bitfield::Specifier>::BITS as usize )* }
                });
            let start_name = f_ident.clone().map(|id| format_ident!("START_{}", id.to_string().to_ascii_uppercase()));
            let getter = f_ident.clone().map(|id| format_ident!("get_{}", id));
            let setter = f_ident.clone().map(|id| format_ident!("set_{}", id));

            quote! {
                #[repr(C)]
                #vis struct #ident {
                    data: [u8; #size >> 3 + ((#size) % 8 != 0) as usize],
                }

                const _: () ={
                    impl #ident {
                        const SIZE: usize = #size >> 3 + ((#size) % 8 != 0) as usize;

                        #vis fn new() -> Self {
                            Self {
                                data: ::std::default::Default::default(),
                            }
                        }

                        #(
                            const #start_name: usize = #start;

                            #viz fn #getter(&self) -> <#ty as ::bitfield::Specifier>::T {
                                <#ty as ::bitfield::Specifier>::get::<{Self::#start_name}, {Self::SIZE}>(&self.data)
                            }

                            #viz fn #setter(&mut self, #f_ident: <#ty as ::bitfield::Specifier>::T) {
                                <#ty as ::bitfield::Specifier>::set::<{Self::#start_name}, {Self::SIZE}>(&mut self.data, #f_ident);
                            }
                        )*
                    }

                    assert!((#size as usize) % 8 == 0, "fields' size should sum up to a multiple of 8.")
                };
            }
        },
        _ => unimplemented!()
    };

    tokens.into()
}

#[proc_macro_derive(BitfieldSpecifier, attributes(bits))]
pub fn derive_bitfield_specifier(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);

  let ident = &input.ident;
  let syn::Data::Enum(e) = &input.data else {
    return syn::Error::new(
      input.ident.span(),
      "Macro does not support any other item than enums."
    ).into_compile_error().into()
  };

  let discriminants = (&e.variants).into_iter().map(|v| {
    if let Some((_, syn::Expr::Lit(syn::ExprLit {
      lit: syn::Lit::Int(d),
      ..
    }))) = &v.discriminant {
      Some(d.base10_parse::<usize>().unwrap())
    } else {
      None
    }
  });
  let variants = (&e.variants).into_iter().map(|v| &v.ident);
  let num_variants = variants.clone().count();

  let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

  let attr_bits = input.attrs
    .iter()
    .filter_map(|attr| match &attr.meta {
      mnv @ syn::Meta::NameValue(syn::MetaNameValue {
        path,
        value: syn::Expr::Lit(syn::ExprLit {
          lit: syn::Lit::Int(val), ..}),
        ..
      }) if path.get_ident()
                .map_or(false, |ident| ident == "bits") =>
        Some((mnv.span(), val.base10_parse::<usize>())),
      _ => None,
    })
  .collect::<Vec<_>>();

  if attr_bits.len() > 1 {
    return syn::Error::new(
        attr_bits[1].0,
        "Only one `#[bits = N]` attribute is allowed.")
      .into_compile_error()
      .into()
  }

  let number_bits = match attr_bits.into_iter().nth(0)
    .map(|(_, val)| val) {
      Some(Ok(val)) => val,
      Some(Err(e)) => return e.into_compile_error().into(),
      _ => {
        discriminants
          .map(|v| match v {
            Some(v) => get_min_number_of_bits(v),
            _ => 0,
          })
          .max()
        .expect("a max value must exists as we iterate over an enum discriminants.")
      }
    };
  let number_bits = (get_min_number_of_bits(num_variants) - 1).max(number_bits);

  let bty = format_ident!("B{}", number_bits);

  let tokens = quote! {
    const _: () = {
      impl #impl_generics ::bitfield::Specifier for #ident #ty_generics #where_clause {
        const BITS: usize = <#bty as ::bitfield::Specifier>::BITS;

        type T = #ident;

        fn set<const ACC: usize, const SIZE: usize>(arr: &mut [u8], val: <Self as ::bitfield::Specifier>::T) {
          <#bty as ::bitfield::Specifier>::set::<ACC, SIZE>(arr, val as <#bty as ::bitfield::Specifier>::T)
        }

        fn get<const ACC: usize, const SIZE: usize>(arr: &[u8]) -> <Self as ::bitfield::Specifier>::T {
          fn __from_unsigned(num: <#bty as ::bitfield::Specifier>::T) -> #ident {
            const VAR_MAP: [(<#bty as ::bitfield::Specifier>::T, #ident); #num_variants] = {
              use #ident::*;
              [#( (#variants as <#bty as ::bitfield::Specifier>::T, #variants) ),*]
            };
            VAR_MAP.into_iter().find_map(|(u, e)| if u == num { Some(e) } else { None }).unwrap()
          }
          __from_unsigned(<#bty as ::bitfield::Specifier>::get::<ACC, SIZE>(arr))
        }
      }
    };
  };
  tokens.into()
}

fn get_min_number_of_bits(mut val: usize) -> usize {
  let mut p = 0;
  while val > 0 { p += 1; val >>= 1; }
  p
}

#[proc_macro]
pub fn gen(_: TokenStream) -> TokenStream {
    fn to_unsigned(bits: usize) -> (proc_macro2::Ident, proc_macro2::Ident) {
      let u = match bits {
        65.. => unreachable!(),
        33.. => 64u8,
        17.. => 32,
        9.. => 16,
        _ => 8
      };
      (format_ident!("u{}", u), format_ident!("BitsU{}", u))
    }
    let range = 1..=64usize;
    let ident = range.clone().map(|n| format_ident!("B{}", n));
    let (u_ident, bits_u): (Vec<_>, Vec<_>) =
      range
        .clone()
        .map(to_unsigned)
        .unzip();

    let tokens = quote! {
        #(
            pub struct #ident;

            impl Specifier for #ident {
                const BITS: usize = #range;
                type T = ::core::primitive::#u_ident;

                fn set<const ACC: usize, const SIZE: usize>(arr: &mut [u8], val: <Self as Specifier>::T) {
                    #bits_u::<{Self::BITS}, ACC, SIZE>::SET(arr, val).unwrap()
                }

                fn get<const ACC: usize, const SIZE: usize>(arr: &[u8]) -> <Self as Specifier>::T {
                    #bits_u::<{Self::BITS}, ACC, SIZE>::GET(arr)
                }
            }
        )*
    };
    tokens.into()
}
