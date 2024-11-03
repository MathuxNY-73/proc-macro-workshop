use proc_macro::TokenStream;
use proc_macro2::{TokenTree as TT, TokenStream as TokenStream2};

use quote::TokenStreamExt;
use syn::buffer::{TokenBuffer, Cursor};
use syn::parse_macro_input;
use syn::parse::{Parse, ParseStream};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as Seq);

    let res = seq.expand().into();
    res
}

#[derive(Debug)]
struct Seq {
    ident: syn::Ident,
    token_in: syn::Token![in],
    lower_bound: syn::LitInt,
    token_2dots: syn::Token![..],
    token_eq: Option<syn::Token![=]>,
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
            token_eq: input.parse().ok(),
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
                let (matched, stream) = Self::partial_match(cursor, &ident, lb, ub);
                if matched {
                    stream
                } else {
                    TokenStream2::from_iter(
                        (lb..ub).into_iter().map(|val| Self::replace(cursor, &ident, val)))
                }
            },
            (Err(mut lb_e), Err(ub)) => {
                lb_e.combine(ub);
                lb_e.into_compile_error()
            }
            (Err(e), _) | (_, Err(e)) => e.into_compile_error(),
        }
    }

    fn partial_match(mut cursor: Cursor, ident: &syn::Ident, lb: usize, ub: usize) -> (bool, TokenStream2) {
        let mut result = TokenStream2::new();
        let mut matched = false;
        while let Some((token, cur)) = cursor.token_tree() {
            cursor = cur;
            match token {
                ref tt @ TT::Punct(ref p) if p.as_char() == '#' => {
                    cursor.clone().token_tree().map_or((), |(t, cur)| {
                        let Some((TT::Punct(p), cur)) = cur.token_tree() else {
                            result.append(tt.clone());
                            return;
                        };
                        if p.as_char() != '*' {
                            result.append(tt.clone());
                            return;
                        }
                        matched = true;
                        match t {
                            TT::Group(g) => {
                                cursor = cur;
                                result.extend((lb..ub).into_iter().map(|val| Self::replace(TokenBuffer::new2(g.stream()).begin(), ident, val)));
                            },
                            _ => result.append(tt.clone()),
                    }})
                },
                TT::Group(ref g) => {
                    let (m, ts) = Self::partial_match(TokenBuffer::new2(g.stream()).begin(), ident, lb, ub);
                    matched = m;
                    let mut new_group = proc_macro2::Group::new(
                        g.delimiter(),
                        ts);
                    new_group.set_span(g.span());
                    result.append(new_group);
                }
                t => result.append(t),
            };
        }
        (matched, result)
    }

    fn replace(mut cursor: Cursor, ident: &syn::Ident, val: usize) -> TokenStream2 {
        let mut result = TokenStream2::new();
        while let Some((token, cur)) = cursor.token_tree() {
            cursor = cur;
            let t = match token {
                TT::Ident(i) if &i == ident => proc_macro2::Literal::usize_unsuffixed(val).into(),
                ref tt @ TT::Ident(ref outter_id) => {
                    cursor.clone().token_tree().map_or(tt.clone(), |(t, cur)| {
                        match t {
                            ref tt @ TT::Punct(ref p) if p.as_char() == '~' => {
                                cursor = cur;
                                cursor.clone().token_tree().map_or(tt.clone(), |(t, cur)| {
                                    match t {
                                        TT::Ident(i) if &i == ident => {
                                            cursor = cur;
                                            quote::format_ident!("{}{}", outter_id, val).into()
                                        },
                                        _ => tt.clone(),
                                    }
                                })
                            },
                            _ => tt.clone(),
                        }
                    })
                },
                TT::Group(ref g) => {
                    let mut new_group = proc_macro2::Group::new(
                        g.delimiter(),
                        Self::replace(TokenBuffer::new2(g.stream()).begin(), ident, val));
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

