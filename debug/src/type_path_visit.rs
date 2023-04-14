use std::collections::HashMap;

use syn::visit::{self, Visit};

pub struct TypePathVisitor {
    pub generic_type_names: Vec<String>,
    pub associate_type_paths: HashMap<String, Vec<syn::TypePath>>,
}

impl<'ast> Visit<'ast> for TypePathVisitor {
    fn visit_type_path(&mut self, node: &'ast syn::TypePath) {
        if node.path.segments.len() >= 2 {
            let generic_type_name = node.path.segments[0].ident.to_string();
            if self.generic_type_names.contains(&generic_type_name) {
                self.associate_type_paths
                    .entry(generic_type_name)
                    .or_insert(Vec::new())
                    .push(node.clone());
            }
        }

        visit::visit_type_path(self, node);
    }
}
