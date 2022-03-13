//! Parse the root tokens in the rsx!{} macro
//! =========================================
//!
//! This parsing path emerges directly from the macro call, with `RsxRender` being the primary entrance into parsing.
//! This feature must support:
//! - [x] Optionally rendering if the `in XYZ` pattern is present
//! - [x] Fragments as top-level element (through ambiguous)
//! - [x] Components as top-level element (through ambiguous)
//! - [x] Tags as top-level elements (through ambiguous)
//! - [x] Good errors if parsing fails
//!
//! Any errors in using rsx! will likely occur when people start using it, so the first errors must be really helpful.

mod component;
mod element;
mod node;

pub mod pretty;

// Re-export the namespaces into each other
pub use component::*;
pub use element::*;
pub use node::*;

// imports
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream},
    Ident, Result, Token, Error
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CustomContext {
    pub name: Ident,
    pub cx_type: Option<Ident>,
}

pub struct CallBody {
    pub custom_context: Option<CustomContext>,
    pub roots: Vec<BodyNode>,
}

impl Parse for CallBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let custom_context = if input.peek(Ident) &&
            input.peek2(Token![:]) &&
            input.peek3(Ident)
        {
            let name = input.parse::<Ident>()?;
            input.parse::<Token![:]>()?;
            let r#type = input.parse::<Ident>()?;
            input.parse::<Token![;]>()?;

            Some(CustomContext { name, cx_type: Some(r#type) })
        } else if input.peek(Ident) && input.peek2(Token![,]) {
            let name = input.parse::<Ident>()?;
            input.parse::<Token![,]>()?;

            Some(CustomContext { name, cx_type: None })
        } else {
            None
        };

        let mut roots = Vec::new();

        while !input.is_empty() {
            let node = input.parse::<BodyNode>()?;

            if input.peek(Token![,]) {
                let _ = input.parse::<Token![,]>();
            }

            roots.push(node);
        }

        Self::update_nodes(&mut roots, custom_context.clone())?;

        Ok(Self {
            custom_context,
            roots,
        })
    }
}

impl CallBody {
    fn update_nodes(nodes: &mut Vec<BodyNode>, context: Option<CustomContext>) -> Result<()> {
        return update_nodes_impl(nodes, &context, None);

        fn update_nodes_impl(
            nodes: &mut Vec<BodyNode>, context: &Option<CustomContext>, parent: Option<Ident>
        ) -> Result<()> {
            for node in nodes {
                match node {
                    BodyNode::Element(element) => {
                        let next_level = build_next_level(&parent, &element.name);

                        element.parent = parent.clone();
                        element.context = (*context).clone();

                        update_nodes_impl(&mut element.children, context, Some(next_level))?;
                    }
                    BodyNode::Component(component) => {
                        let name = component.name.get_ident().ok_or_else(
                            || Error::new(Span::call_site(), "Only identifier components support, using a path is invalid")
                        )?;
                        let next_level = build_next_level(&parent, &name);

                        component.parent = parent.clone();
                        component.context = (*context).clone();

                        update_nodes_impl(&mut component.children, context, Some(next_level))?;
                    }
                    _ => {}
                }
            }

            return Ok(());

            fn build_next_level(parent: &Option<Ident>, name: &Ident) -> Ident {
                match parent {
                    Some(parent) => Ident::new(&format!("{parent}_{}", name), Span::call_site()),
                    None => name.clone()
                }
            }
        }
    }
}
/// Serialize the same way, regardless of flavor
impl ToTokens for CallBody {
    fn to_tokens(&self, out_tokens: &mut TokenStream2) {
        let inner = if self.roots.len() == 1 {
            let inner = &self.roots[0];
            quote! { #inner }
        } else {
            let childs = &self.roots;
            quote! { __cx.fragment_root([ #(#childs),* ]) }
        };

        match &self.custom_context {
            // The `in cx` pattern allows directly rendering
            Some(CustomContext { name, .. }) => out_tokens.append_all(quote! {
                #name.render(LazyNodes::new_some(move |__cx: NodeFactory| -> VNode {
                    use dioxus_elements::{GlobalAttributes, SvgAttributes};
                    #inner
                }))
            }),

            // Otherwise we just build the LazyNode wrapper
            None => out_tokens.append_all(quote! {
                LazyNodes::new_some(move |__cx: NodeFactory| -> VNode {
                    use dioxus_elements::{GlobalAttributes, SvgAttributes};
                    #inner
                })
            }),
        };
    }
}
