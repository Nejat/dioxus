use std::mem::take;
use proc_macro2::{TokenStream as TokenStream2, TokenStream};
use proc_macro_error::abort_call_site;
use quote::{quote, TokenStreamExt, ToTokens};
use syn::{braced, bracketed, Expr, Ident, LitStr, parenthesized, parse::{Parse, ParseBuffer, ParseStream}, Result, Token, token};

use super::*;

// =======================================
// Parse the VNode::Element type
// =======================================
pub struct Element {
    pub name: Ident,
    pub key: Option<LitStr>,
    pub attributes: Vec<ElementAttrNamed>,
    pub children: Vec<BodyNode>,
    pub _is_static: bool,
}

impl Parse for Element {
    fn parse(stream: ParseStream) -> Result<Self> {
        let el_name = Ident::parse(stream)?;

        // parse the guts
        let content: ParseBuffer;
        syn::braced!(content in stream);

        let mut attributes: Vec<ElementAttrNamed> = vec![];
        let mut children: Vec<BodyNode> = vec![];
        let mut key = None;
        let mut _el_ref = None;

        // parse fields with commas
        // break when we don't get this pattern anymore
        // start parsing bodynodes
        // "def": 456,
        // abc: 123,
        loop {
            // Parse the raw literal fields
            if content.peek(LitStr) && content.peek2(Token![:]) && !content.peek3(Token![:]) {
                let name = content.parse::<LitStr>()?;
                let ident = name.clone();

                content.parse::<Token![:]>()?;

                if content.peek(LitStr) && content.peek2(Token![,]) {
                    let value = content.parse::<LitStr>()?;
                    attributes.push(ElementAttrNamed {
                        el_name: el_name.clone(),
                        attr: ElementAttr::CustomAttrText { name, value },
                    });
                } else {
                    let value = content.parse::<Expr>()?;

                    attributes.push(ElementAttrNamed {
                        el_name: el_name.clone(),
                        attr: ElementAttr::CustomAttrExpression { name, value },
                    });
                }

                if content.is_empty() {
                    break;
                }

                // todo: add a message saying you need to include commas between fields
                if content.parse::<Token![,]>().is_err() {
                    proc_macro_error::emit_error!(
                        ident,
                        "This attribute is missing a trailing comma"
                    )
                }
                continue;
            }

            if content.peek(Ident) && content.peek2(Token![!]) {
                let macro_name = content.parse::<Ident>()?;

                content.parse::<Token![!]>()?;

                let macro_content;

                if content.peek(token::Paren) {
                    parenthesized!(macro_content in content);
                } else if content.peek(token::Bracket) {
                    bracketed!(macro_content in content);
                } else if content.peek(token::Brace) {
                    braced!(macro_content in content);
                } else {
                    abort_call_site!(format!("'{macro_name}' is an invalid macro invocation"))
                }

                let macro_type = MacroType::try_from(macro_content.parse::<Ident>()?)?;

                macro_content.parse::<token::FatArrow>()?;

                let macro_expr = macro_content.parse::<TokenStream>()?;

                if !macro_content.is_empty() {
                    abort_call_site!(format!("Unexpected macro content: {macro_content}"));
                }

                attributes.push(ElementAttrNamed {
                    el_name: el_name.clone(),
                    attr: ElementAttr::Macro { name: macro_name, macro_type, macro_expr },
                });

                if content.is_empty() {
                    break;
                }

                continue;
            }

            if content.peek(Ident) && content.peek2(Token![:]) && !content.peek3(Token![:]) {
                let name = content.parse::<Ident>()?;
                let ident = name.clone();

                let name_str = name.to_string();
                content.parse::<Token![:]>()?;

                if name_str.starts_with("on") {
                    attributes.push(ElementAttrNamed {
                        el_name: el_name.clone(),
                        attr: ElementAttr::EventTokens {
                            name,
                            tokens: content.parse()?,
                        },
                    });
                } else {
                    match name_str.as_str() {
                        "key" => {
                            key = Some(content.parse()?);
                        }
                        "classes" => todo!("custom class list not supported yet"),
                        // "namespace" => todo!("custom namespace not supported yet"),
                        "node_ref" => {
                            _el_ref = Some(content.parse::<Expr>()?);
                        }
                        _ => {
                            if content.peek(LitStr) {
                                attributes.push(ElementAttrNamed {
                                    el_name: el_name.clone(),
                                    attr: ElementAttr::AttrText {
                                        name,
                                        value: content.parse()?,
                                    },
                                });
                            } else {
                                attributes.push(ElementAttrNamed {
                                    el_name: el_name.clone(),
                                    attr: ElementAttr::AttrExpression {
                                        name,
                                        value: content.parse()?,
                                    },
                                });
                            }
                        }
                    }
                }

                if content.is_empty() {
                    break;
                }

                // todo: add a message saying you need to include commas between fields
                if content.parse::<Token![,]>().is_err() {
                    proc_macro_error::emit_error!(
                        ident,
                        "This attribute is missing a trailing comma"
                    )
                }
                continue;
            }

            break;
        }

        while !content.is_empty() {
            if (content.peek(LitStr) && content.peek2(Token![:])) && !content.peek3(Token![:]) {
                let ident = content.parse::<LitStr>().unwrap();
                let name = ident.value();
                proc_macro_error::emit_error!(
                    ident, "This attribute `{}` is in the wrong place.", name;
                    help =
"All attribute fields must be placed above children elements.

                div {
                   attr: \"...\",  <---- attribute is above children
                   div { }       <---- children are below attributes
                }";
                )
            }

            if (content.peek(Ident) && content.peek2(Token![:])) && !content.peek3(Token![:]) {
                let ident = content.parse::<Ident>().unwrap();
                let name = ident.to_string();
                proc_macro_error::emit_error!(
                    ident, "This attribute `{}` is in the wrong place.", name;
                    help =
"All attribute fields must be placed above children elements.

                div {
                   attr: \"...\",  <---- attribute is above children
                   div { }       <---- children are below attributes
                }";
                )
            }

            children.push(content.parse::<BodyNode>()?);
            // consume comma if it exists
            // we don't actually care if there *are* commas after elements/text
            if content.peek(Token![,]) {
                let _ = content.parse::<Token![,]>();
            }
        }

        Ok(Self {
            key,
            name: el_name,
            attributes,
            children,
            _is_static: false,
        })
    }
}

impl ToTokens for Element {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let name = &self.name;
        let children = &self.children;

        let key = match &self.key {
            Some(ty) => quote! { Some(format_args_f!(#ty)) },
            None => quote! { None },
        };

        let listeners = self
            .attributes
            .iter()
            .filter(|f| matches!(f.attr, ElementAttr::EventTokens { .. }));

        let macro_listeners = self
            .attributes
            .iter()
            .filter(|f| matches!(f.attr, ElementAttr::Macro { macro_type, .. } if macro_type == MacroType::Listeners))
            .take(1)
            .next();

        let attr = self
            .attributes
            .iter()
            .filter(|f| !matches!(f.attr, ElementAttr::EventTokens { .. }) && !matches!(f.attr, ElementAttr::Macro { .. }));

        let macro_attr = self
            .attributes
            .iter()
            .filter(|f| matches!(f.attr, ElementAttr::Macro { macro_type, .. } if macro_type == MacroType::Attributes));

        // __cx.bump().alloc(#macro_attr), // this came close, but would have needed to incorporate #(#attr),* was creating unnecessary extra outer brackets

        let elements = quote! {
            __cx.element(
                dioxus_elements::#name,
                __cx.bump().alloc([ #(#listeners),* #(#macro_listeners)* ]),
                __cx.bump().alloc([ #(#attr),* #(#macro_attr)* ]),
                __cx.bump().alloc([ #(#children),* ]),
                #key,
            )
        };

        // println!("\nDIOXUS: {elements}\n");

        tokens.append_all(elements);
    }
}

///
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MacroType {
    /// macro generates listeners
    Listeners,

    /// macro generates attributes
    Attributes,
}

impl TryFrom<Ident> for MacroType {
    type Error = syn::Error;

    fn try_from(value: Ident) -> std::result::Result<Self, Self::Error> {
        match value.to_string().as_str() {
            "events" => Ok(Self::Listeners),
            "attributes" => Ok(Self::Attributes),
            unsupported => Err(Self::Error::new_spanned(value, format!("{unsupported:?} is not a supported macro plugin type")))
        }
    }
}

pub enum ElementAttr {
    /// attribute: "valuee {}"
    AttrText { name: Ident, value: LitStr },

    /// attribute: true,
    AttrExpression { name: Ident, value: Expr },

    /// "attribute": "value {}"
    CustomAttrText { name: LitStr, value: LitStr },

    /// "attribute": true,
    CustomAttrExpression { name: LitStr, value: Expr },

    // /// onclick: move |_| {}
    // EventClosure { name: Ident, closure: ExprClosure },
    /// onclick: {}
    EventTokens { name: Ident, tokens: Expr },

    ///
    Macro { name: Ident, macro_type: MacroType, macro_expr: TokenStream },
}

pub struct ElementAttrNamed {
    pub el_name: Ident,
    pub attr: ElementAttr,
}

impl ToTokens for ElementAttrNamed {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let ElementAttrNamed { el_name, attr } = self;

        tokens.append_all(match attr {
            ElementAttr::AttrText { name, value } => {
                quote! {
                    dioxus_elements::#el_name.#name(__cx, format_args_f!(#value))
                }
            }
            ElementAttr::AttrExpression { name, value } => {
                quote! {
                    dioxus_elements::#el_name.#name(__cx, #value)
                }
            }
            ElementAttr::CustomAttrText { name, value } => {
                quote! {
                    __cx.attr( #name, format_args_f!(#value), None, false )
                }
            }
            ElementAttr::CustomAttrExpression { name, value } => {
                quote! {
                    __cx.attr( #name, format_args_f!(#value), None, false )
                }
            }
            // ElementAttr::EventClosure { name, closure } => {
            //     quote! {
            //         dioxus_elements::on::#name(__cx, #closure)
            //     }
            // }
            ElementAttr::EventTokens { name, tokens } => {
                quote! {
                    dioxus_elements::on::#name(__cx, #tokens)
                }
            }
            ElementAttr::Macro { name: macro_name, macro_type, macro_expr } => {
                match macro_type {
                    MacroType::Listeners => {
                        quote! {
                            #macro_name! ( #macro_expr )
                        }
                    }
                    MacroType::Attributes => {
                        quote! {
                            #macro_name! ( #el_name; #macro_expr )
                        }
                    }
                }
            }
        });
    }
}
