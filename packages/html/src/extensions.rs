//! Manual implementations because they contain "volatile" values
//!
use crate::builder::{ElementBuilder, IntoAttributeValue};

impl<'a> ElementBuilder<'a, crate::builder::elements::Input::Input> {
    pub fn value(mut self, val: impl IntoAttributeValue<'a>) -> Self {
        self.push_attr_volatile("value", val);
        self
    }
}

impl<'a> ElementBuilder<'a, crate::builder::elements::Select::Select> {
    pub fn value(mut self, val: impl IntoAttributeValue<'a>) -> Self {
        self.push_attr_volatile("value", val);
        self
    }
}

impl<'a> ElementBuilder<'a, crate::builder::elements::Option_::Option_> {
    pub fn selected(mut self, val: impl IntoAttributeValue<'a>) -> Self {
        self.push_attr_volatile("selected", val);
        self
    }
}

impl<'a> ElementBuilder<'a, crate::builder::elements::Textarea::Textarea> {
    pub fn value(mut self, val: impl IntoAttributeValue<'a>) -> Self {
        self.push_attr_volatile("value", val);
        self
    }
}