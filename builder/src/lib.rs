extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{parse_macro_input, Data, DeriveInput, Field, Fields, Ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;

    let fields = match input.data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => &fields.named,
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());

    let builder_struct = impl_builder_struct(&builder_name, fields);
    let build_struct_setters = impl_builder_struct_setters(&builder_name, fields);
    let build_struct_builder = impl_builder_struct_build(name, &builder_name, fields);

    let builder_fn = impl_builder_fn(name, &builder_name, fields);

    let expanded = quote! {
        #builder_struct
        #build_struct_setters
        #build_struct_builder
        #builder_fn

    };
    TokenStream::from(expanded)
}

fn impl_builder_struct(
    name: &Ident,
    fields: &Punctuated<Field, Comma>,
) -> proc_macro2::TokenStream {
    let new_fields_iter = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! {
         #name: std::option::Option<#ty>
        }
    });
    let expanded = quote! {
        pub struct #name {
            #( #new_fields_iter ),*
        }
    };
    expanded
}

fn impl_builder_struct_setters(
    name: &Ident,
    fields: &Punctuated<Field, Comma>,
) -> proc_macro2::TokenStream {
    let setters_iter = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! {
            fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });
    let expanded = quote! {
        impl #name {
            #( #setters_iter )*
        }
    };
    expanded
}

fn impl_builder_struct_build(
    name: &Ident,
    builder_name: &Ident,
    fields: &Punctuated<Field, Comma>,
) -> proc_macro2::TokenStream {
    let setters_iter = fields.iter().map(|f| {
        let name = &f.ident;
        quote! {
            #name: self.#name.take().unwrap()
        }
    });
    let expanded = quote! {
        impl #builder_name {
             pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                 Ok(
                    #name {
                        #( #setters_iter ),*
                    }
                 )
             }
        }
    };
    expanded
}

fn impl_builder_fn(
    name: &Ident,
    builder_name: &Ident,
    fields: &Punctuated<Field, Comma>,
) -> proc_macro2::TokenStream {
    let new_fields_iter = fields.iter().map(|f| {
        let name = &f.ident;
        quote! {
         #name: None
        }
    });
    let expanded = quote! {

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                     #( #new_fields_iter ),*
                }
            }
        }
    };
    expanded
}
