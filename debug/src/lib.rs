use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, punctuated::Punctuated, DeriveInput, Field, Token};

type StructFileds = Punctuated<Field, Token![,]>;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    match do_expand(&ast) {
        Ok(tkn) => tkn.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(ast: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_fields_from_derive_input(ast)?;

    let impl_debug_trait_clause = generate_implements_dubug_trait(ast, fields)?;

    let ret = quote!(
        #impl_debug_trait_clause
    );
    Ok(ret)
}

fn get_fields_from_derive_input(ast: &DeriveInput) -> syn::Result<&StructFileds> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        Ok(named)
    } else {
        Err(syn::Error::new_spanned(
            ast,
            "Must define on struct, Not on enum.",
        ))
    }
}

fn get_user_specified_debug_format(field: &Field) -> syn::Result<std::option::Option<String>> {
    for attr in field.attrs.iter() {
        if let Ok(syn::Meta::NameValue(ref kv)) = attr.parse_meta() {
            if kv.path.is_ident("debug") {
                if let syn::Lit::Str(ref lit_str) = kv.lit {
                    return Ok(Some(lit_str.value().to_string()));
                }
            }
        }
    }
    Ok(None)
}

fn generate_implements_dubug_trait(
    ast: &DeriveInput,
    fields: &StructFileds,
) -> syn::Result<proc_macro2::TokenStream> {
    let mut field_set_clause = proc_macro2::TokenStream::new();
    let struct_name_ident = &ast.ident;

    for field in fields.iter() {
        let field_name = &field.ident;
        let clause = if let Some(format) = get_user_specified_debug_format(field)? {
            quote!(
                .field(stringify!(#field_name), &format_args!(#format, self.#field_name))
            )
        } else {
            quote!(
                .field(stringify!(#field_name), &self.#field_name)
            )
        };
        field_set_clause.extend(clause);
    }

    let ret = quote!(
        impl std::fmt::Debug for #struct_name_ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#struct_name_ident))
                    #field_set_clause
                    .finish()
            }
        }
    );

    Ok(ret)
}
