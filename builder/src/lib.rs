use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{
    self, parse_macro_input, punctuated::Punctuated, spanned::Spanned, DeriveInput, Field, Token,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    match do_expand(&ast) {
        Ok(tkn) => tkn.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(ast: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_literal = ast.ident.to_string();
    let builder_name_literal = format!("{}Builder", struct_name_literal);
    let builder_name_ident = syn::Ident::new(&builder_name_literal, ast.span());

    let struct_ident = &ast.ident;

    let fields = get_fileds_from_derive_input(ast)?;
    let builder_fileds_def = generate_builder_struct_fields_def(fields)?;
    let builder_constructor_clauses = generate_builder_constructor_clauses(fields)?;
    let builder_setters = generate_builder_setters(fields)?;
    let build_function = generate_build_function(struct_ident, fields)?;

    let ret = quote!(
        pub struct #builder_name_ident {
            #builder_fileds_def
        }

        impl #struct_ident {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident {
                    #builder_constructor_clauses
                }
            }
        }

        impl #builder_name_ident {
            #builder_setters

            #build_function
        }
    );
    Ok(ret)
}

type StructFileds = Punctuated<Field, Token![,]>;

fn get_fileds_from_derive_input(ast: &DeriveInput) -> syn::Result<&StructFileds> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        Ok(named)
    } else {
        Err(syn::Error::new_spanned(
            ast,
            "Must define on struct, Not on enum",
        ))
    }
}

fn generate_builder_struct_fields_def(
    fields: &StructFileds,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields
        .iter()
        .map(|f| {
            if let Some(inner_type) = get_optional_inner_type(&f.ty) {
                inner_type
            } else {
                &f.ty
            }
        })
        .collect();

    let ret = quote!(
        #( #idents: std::option::Option<#types> ),*
    );
    Ok(ret)
}

fn generate_builder_constructor_clauses(
    fields: &StructFileds,
) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();

    let ret = quote!(
        #( #idents: std::option::Option::None ),*
    );
    Ok(ret)
}

fn generate_builder_setters(fields: &StructFileds) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<_> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let mut token_stream = proc_macro2::TokenStream::new();

    for (ident, ty) in idents.iter().zip(types.iter()) {
        let clause = if let Some(inner_ty) = get_optional_inner_type(ty) {
            quote!(
                pub fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            )
        } else {
            quote!(
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            )
        };

        token_stream.extend(clause);
    }

    Ok(token_stream)
}

fn generate_build_function(
    struct_ident: &Ident,
    fields: &StructFileds,
) -> syn::Result<proc_macro2::TokenStream> {
    let mut check_code_clause = proc_macro2::TokenStream::new();
    let mut fileds_generate_clause = proc_macro2::TokenStream::new();

    fields.iter().for_each(|f| {
        let ident = &f.ident;
        if let None = get_optional_inner_type(&f.ty) {
            let check_piece = quote!(
                if self.#ident.is_none() {
                    let err = format!("{}: filed is missing.", stringify!(#ident));
                    return std::result::Result::Err(err.into())
                }
            );

            let generate_piece = quote!(
                #ident: self.#ident.clone().unwrap(),
            );

            check_code_clause.extend(check_piece);
            fileds_generate_clause.extend(generate_piece);
        } else {
            let generate_piece = quote!(
                #ident: self.#ident.clone(),
            );
            fileds_generate_clause.extend(generate_piece);
        }
    });

    let ret = quote!(
        pub fn build(&mut self) -> std::result::Result<#struct_ident, Box<dyn std::error::Error>> {
            #check_code_clause

            let ret = #struct_ident {
                #fileds_generate_clause
            };
            std::result::Result::Ok(ret)
        }
    );
    Ok(ret)
}

fn get_optional_inner_type(t: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path { segments, .. },
    }) = t
    {
        if let Some(seg) = segments.last() {
            if seg.ident.to_string() == "Option" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) = seg.arguments
                {
                    if let Some(syn::GenericArgument::Type(inner_type)) = args.first() {
                        return Some(inner_type);
                    }
                }
            }
        }
        None
    } else {
        None
    }
}
