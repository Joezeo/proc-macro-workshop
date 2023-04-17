use proc_macro::TokenStream;
use quote::quote;
use syn::{self, parse::Parse, parse_macro_input, Token};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as SeqParser);

    let mut ret = proc_macro2::TokenStream::new();

    for i in ast.start..ast.end {
        ret.extend(ast.expand(&ast.body, i))
    }

    ret.into()
}

struct SeqParser {
    variable_ident: syn::Ident,
    start: isize,
    end: isize,
    body: proc_macro2::TokenStream,
}

/// N in xx..xx {.......}
impl Parse for SeqParser {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let variable_ident: syn::Ident = input.parse()?;

        input.parse::<Token!(in)>()?;

        let start_lit: syn::LitInt = input.parse()?;

        input.parse::<Token!(..)>()?;

        let end_lit: syn::LitInt = input.parse()?;

        let body_buf;
        syn::braced!(body_buf in input);
        let body: proc_macro2::TokenStream = body_buf.parse()?;

        Ok(Self {
            variable_ident,
            start: start_lit.base10_parse()?,
            end: end_lit.base10_parse()?,
            body,
        })
    }
}

impl SeqParser {
    fn expand(&self, ts: &proc_macro2::TokenStream, n: isize) -> proc_macro2::TokenStream {
        let buf = ts.clone().into_iter().collect::<Vec<_>>();
        let mut ret = proc_macro2::TokenStream::new();

        for idx in 0..buf.len() {
            let tree_node = &buf[idx];
            match tree_node {
                proc_macro2::TokenTree::Group(g) => {
                    // 递归展开内部的TokenStream
                    let new_stream = self.expand(&g.stream(), n);
                    // 通过 g.stream() 生成内部的TokenStream会去除掉本身的括号
                    // 在进行 self.expand() 之后需要重新套上一层括号, 括号的种类需要和之前的一致
                    let wrap_in_group = proc_macro2::Group::new(g.delimiter(), new_stream);
                    ret.extend(quote!(#wrap_in_group));
                }
                proc_macro2::TokenTree::Ident(i) => {
                    // 判断是否是之前定义的变量标识符, 如果是, 则替换成一个 Literal
                    if i == &self.variable_ident {
                        let new_ident = proc_macro2::Literal::i64_unsuffixed(n as i64);
                        ret.extend(quote!(#new_ident));
                    } else {
                        ret.extend(quote!(#tree_node));
                    }
                }
                _ => ret.extend(quote!(#tree_node)),
            }
        }

        ret
    }
}
