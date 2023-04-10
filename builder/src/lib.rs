use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{
    self, parse_macro_input, punctuated::Punctuated, spanned::Spanned, DeriveInput, Field, Token,
};

#[proc_macro_derive(Builder, attributes(builder))]
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
    let mut token = proc_macro2::TokenStream::new();
    for f in fields.iter() {
        let ident = &f.ident;
        let clause = if let Some(inner_type) = get_generic_inner_type(&f.ty, "Option") {
            quote!(
                #ident: std::option::Option<#inner_type>,
            )
        } else if get_user_specified_ident_for_vec(&f)?.is_some() {
            let ty = &f.ty;
            quote!(
                #ident: #ty,
            )
        } else {
            let ty = &f.ty;
            quote!(
                #ident: std::option::Option<#ty>,
            )
        };
        token.extend(clause);
    }

    Ok(token)
}

fn generate_builder_constructor_clauses(
    fields: &StructFileds,
) -> syn::Result<proc_macro2::TokenStream> {
    let mut token = proc_macro2::TokenStream::new();
    for f in fields.iter() {
        let ident = &f.ident;
        let clause = if get_user_specified_ident_for_vec(&f)?.is_some() {
            quote!(
                #ident: std::vec::Vec::new(),
            )
        } else {
            quote!(
                #ident: std::option::Option::None,
            )
        };
        token.extend(clause);
    }

    Ok(token)
}

fn generate_builder_setters(fields: &StructFileds) -> syn::Result<proc_macro2::TokenStream> {
    let mut token_stream = proc_macro2::TokenStream::new();

    for field in fields.iter() {
        let ident = &field.ident;
        let ty = &field.ty;
        let clause = if let Some(inner_ty) = get_generic_inner_type(ty, "Option") {
            quote!(
                pub fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            )
        } else if let Some(single_ident) = get_user_specified_ident_for_vec(field)? {
            let inner_type = get_generic_inner_type(ty, "Vec").ok_or(syn::Error::new(
                field.span(),
                "`each` filed must be a Vec type.",
            ))?;
            let mut clause = quote!(
                pub fn #single_ident(&mut self, #single_ident: #inner_type) -> &mut Self {
                    self.#ident.push(#single_ident);
                    self
                }
            );
            if ident.as_ref().unwrap().to_string() != single_ident.to_string() {
                clause.extend(quote!(
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = #ident;
                        self
                    }
                ))
            }
            clause
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

    for f in fields.iter() {
        let ident = &f.ident;
        if get_generic_inner_type(&f.ty, "Option").is_none()
            && get_user_specified_ident_for_vec(f)?.is_none()
        {
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
    }

    let ret = quote!(
        pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #check_code_clause

            let ret = #struct_ident {
                #fileds_generate_clause
            };
            std::result::Result::Ok(ret)
        }
    );
    Ok(ret)
}

fn get_generic_inner_type<'a>(t: &'a syn::Type, outter_ident: &str) -> core::option::Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path { segments, .. },
    }) = t
    {
        if let Some(seg) = segments.last() {
            if seg.ident.to_string() == outter_ident {
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

fn get_user_specified_ident_for_vec(field: &Field) -> syn::Result<Option<Ident>> {
    for attr in &field.attrs {
        if let Ok(syn::Meta::List(syn::MetaList {
            ref path,
            ref nested,
            ..
        })) = attr.parse_meta()
        {
            if let Some(p) = path.segments.first() {
                if p.ident == "builder" {
                    if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(kv))) = nested.first() {
                        if kv.path.is_ident("each") {
                            if let syn::Lit::Str(ref lit_str) = kv.lit {
                                return Ok(core::option::Option::Some(Ident::new(lit_str.value().as_str(), attr.span())));
                            }
                        } else {
                            if let Ok(syn::Meta::List(ref list)) = attr.parse_meta() {
                                return Err(syn::Error::new_spanned(
                                    list,
                                    r#"expected `builder(each = "...")`"#,
                                ));
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(core::option::Option::None)
}
