use proc_macro::TokenStream;

use quote::{quote, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, visit_mut::{self, VisitMut}, Item};


struct SortedVisitor {
    is_sorted: bool,
    arm_idents: Vec<syn::Ident>,
    error_agg: Option<syn::Error>,
    current_error: Option<syn::Error>,
}

fn is_sorted_attribute(attr: &syn::Attribute) -> bool {
    match &attr.meta {
        syn::Meta::Path(syn::Path {
            segments,
            ..
        }) => segments.len() == 1 || segments.first().map_or(false, |s| s.ident == "sorted"),
         _ => false,
    }
}

impl visit_mut::VisitMut for SortedVisitor {
  fn visit_expr_match_mut(&mut self, e: &mut syn::ExprMatch) {
    self.is_sorted = e.attrs.iter().any(is_sorted_attribute);
    if self.is_sorted {
        e.attrs = std::mem::take(&mut e.attrs).into_iter().filter(|attr| !is_sorted_attribute(attr)).collect::<Vec<_>>();
    }

    visit_mut::visit_expr_match_mut(self, e);

    if self.is_sorted && self.current_error.is_none() {
        validate_ident_order(&std::mem::take(&mut self.arm_idents))
            .unwrap_or_else(|e| self.current_error = Some(e));
    }
    self.arm_idents.clear();

    match (self.current_error.take(), self.error_agg.as_mut()) {
        (Some(e), Some(agg)) => agg.combine(e),
        (Some(e), _) => self.error_agg = Some(e),
        _ => (),
    }
  }

  fn visit_arm_mut(&mut self, a: &mut syn::Arm) {
      if self.is_sorted {
        match a.pat {
            syn::Pat::TupleStruct(syn::PatTupleStruct {
                path: syn::Path { ref segments, .. },
                ..
            }) if segments.len() == 1 => self.arm_idents.push(segments.first().unwrap().ident.clone()),
            ref p @ _ => {
                let err = syn::Error::new(p.span(), format!("{:#?} pattern is not supported by sorted.", p));
                if let Some(e) = self.current_error.as_mut() {
                    e.combine(err);
                } else {
                    self.current_error = Some(err);
                }
            },
        }
      }

      visit_mut::visit_arm_mut(self, a);
  }
}


#[proc_macro_attribute]
pub fn check(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as syn::ItemFn);

    let mut sorted_visitor = SortedVisitor {
        is_sorted: false,
        arm_idents: Vec::new(),
        error_agg: None,
        current_error: None,
    };
    sorted_visitor.visit_item_fn_mut(&mut input);

    let input = input.to_token_stream();
    let tokens = if let Some(e) = sorted_visitor.error_agg {
        let err = e.into_compile_error();
        quote! { #input #err }
    } else {
        input
    };
    tokens.into()
}

#[proc_macro_attribute]
pub fn sorted(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Item);

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
