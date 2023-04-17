mod type_path_visit;

use std::collections::HashMap;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, visit::Visit, DeriveInput, Field,
    Token, TypePath,
};
use type_path_visit::TypePathVisitor;

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

fn get_user_specified_debug_format(field: &Field) -> syn::Result<Option<String>> {
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

fn get_phantomdata_generic_type_name(field: &Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = field.ty
    {
        if let Some(syn::PathSegment { ident, arguments }) = segments.last() {
            if ident == "PhantomData" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }) = arguments
                {
                    if let Some(syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                        path,
                        ..
                    }))) = args.first()
                    {
                        if let Some(generic_ident) = path.segments.first() {
                            return Ok(Some(generic_ident.ident.to_string()));
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}

fn get_field_type_name(field: &Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = field.ty
    {
        if let Some(syn::PathSegment { ident, .. }) = segments.last() {
            return Ok(Some(ident.to_string()));
        }
    }
    Ok(None)
}

fn get_generic_associate_types(ast: &DeriveInput) -> syn::Result<HashMap<String, Vec<TypePath>>> {
    let origin_generic_param_names: Vec<String> = ast
        .generics
        .params
        .iter()
        .filter_map(|f| {
            if let syn::GenericParam::Type(ty) = f {
                return Some(ty.ident.to_string());
            }
            None
        })
        .collect();

    let mut visitor = TypePathVisitor {
        generic_type_names: origin_generic_param_names,
        associate_type_paths: HashMap::new(),
    };

    visitor.visit_derive_input(ast);
    return Ok(visitor.associate_type_paths);
}

fn get_struct_escape_hatch(ast: &DeriveInput) -> Option<String> {
    if let Some(attr) = ast.attrs.last() {
        if let Ok(syn::Meta::List(syn::MetaList { nested, .. })) = attr.parse_meta() {
            let mut nested_iter = nested.iter();
            while let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(path_value))) =
                nested_iter.next()
            {
                if path_value.path.is_ident("bound") {
                    if let syn::Lit::Str(ref lit) = path_value.lit {
                        return Some(lit.value());
                    }
                }
            }
        }
    }
    None
}

fn generate_implements_dubug_trait(
    ast: &DeriveInput,
    fields: &StructFileds,
) -> syn::Result<proc_macro2::TokenStream> {
    let mut field_set_clause = proc_macro2::TokenStream::new();
    let struct_name_ident = &ast.ident;

    let mut phantomdata_generic_types = vec![];
    let mut struct_field_types = vec![];

    let generic_associate_types = get_generic_associate_types(ast)?;
    for field in fields.iter() {
        if let Some(generic_type) = get_phantomdata_generic_type_name(field)? {
            phantomdata_generic_types.push(generic_type)
        }
        if let Some(field_type) = get_field_type_name(field)? {
            struct_field_types.push(field_type)
        }
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

    let mut generic_params = ast.generics.clone();

    if let Some(escape_hatch) = get_struct_escape_hatch(ast) {
        generic_params.make_where_clause();
        generic_params
            .where_clause
            .as_mut()
            .unwrap()
            .predicates
            .push(syn::parse_str(&escape_hatch).unwrap());
    } else {
        for g in generic_params.params.iter_mut() {
            if let syn::GenericParam::Type(t) = g {
                let type_param_name = t.ident.to_string();
                if phantomdata_generic_types.contains(&type_param_name)
                    && !struct_field_types.contains(&type_param_name)
                {
                    continue;
                }

                if generic_associate_types.contains_key(&type_param_name)
                    && !struct_field_types.contains(&type_param_name)
                {
                    continue;
                }
                t.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }

        // 关联类型的约束要放入where语句中
        generic_params.make_where_clause();
        for (_, type_paths) in generic_associate_types.iter() {
            for ty in type_paths {
                generic_params
                    .where_clause
                    .as_mut()
                    .unwrap()
                    .predicates
                    .push(parse_quote!(#ty: std::fmt::Debug))
            }
        }
    }

    let (impl_generics, type_generics, where_clause) = generic_params.split_for_impl();

    let ret = quote!(
        impl #impl_generics std::fmt::Debug for #struct_name_ident #type_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#struct_name_ident))
                    #field_set_clause
                    .finish()
            }
        }
    );

    Ok(ret)
}
