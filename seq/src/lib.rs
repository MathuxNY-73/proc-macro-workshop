use std::io::BufWriter;

use proc_macro::TokenStream;
use proc_macro2::{TokenTree as TT, TokenStream as TokenStream2};

use quote::{quote, ToTokens, TokenStreamExt};
use syn::buffer::{TokenBuffer, Cursor};
use syn::parse_macro_input;
use syn::parse::{Parse, ParseStream};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as Seq);

    eprintln!("{:#?}", seq);

    seq.expand().into()
}

#[derive(Debug)]
struct Seq {
    ident: syn::Ident,
    token_in: syn::Token![in],
    lower_bound: syn::LitInt,
    token_2dots: syn::Token![..],
    upper_bound: syn::LitInt,
    brace_token: syn::token::Brace,
    content: TokenStream2,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Seq {
            ident: input.parse()?,
            token_in: input.parse()?,
            lower_bound: input.parse()?,
            token_2dots: input.parse()?,
            upper_bound: input.parse()?,
            brace_token: syn::braced!(content in input),
            content: content.parse()?,
        })
    }
}

impl Seq {
    fn expand(self) -> TokenStream2 {
        let Self {
            ident,
            lower_bound,
            upper_bound,
            content,
            ..
        } = self;

        let buffer = TokenBuffer::new2(content);
        let cursor = buffer.begin();

        match (lower_bound.base10_parse(), upper_bound.base10_parse()) {
            (Ok(lb), Ok(ub)) => {
                TokenStream2::from_iter(
                    (lb..ub).into_iter().map(|val| Self::replace(cursor, &ident, proc_macro2::Literal::i32_unsuffixed(val))))
            },
            (Err(mut lb_e), Err(ub)) => {
                lb_e.combine(ub);
                lb_e.into_compile_error()
            }
            (Err(e), _) | (_, Err(e)) => e.into_compile_error(),
        }
    }

    fn replace(mut cursor: Cursor, ident: &syn::Ident, lit: proc_macro2::Literal) -> TokenStream2 {
        let mut result = TokenStream2::new();
        while let Some((token, cur)) = cursor.token_tree() {
            cursor = cur;
            let t = match token {
                TT::Ident(i) if &i == ident => lit.clone().into(),
                TT::Group(ref g) => {
                    let mut new_group = proc_macro2::Group::new(
                        g.delimiter(),
                        Self::replace(TokenBuffer::new2(g.stream()).begin(), ident, lit.clone()));
                    new_group.set_span(g.span());
                    new_group.into()
                    },
                t => t,
            };
            result.append(t);
        }
        result
    }
}

