use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    match do_expand(&ast) {
        Ok(tkn) => tkn.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(ast: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let ret = quote!(

    );
    Ok(ret)
}