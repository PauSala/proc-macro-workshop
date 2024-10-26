use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, token::Comma, DeriveInput, Field,
    GenericParam, Generics, PathArguments, PathSegment,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;

    let struct_name = format!("{}", name);
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        // TODO: return appropiate compile error
        unimplemented!();
    };

    // Heuristic 1:
    // Generics in PhantomData are omited (test case 4);
    // So find them to filter out.
    let mut omits = identify_phantom_data_generics(&fields);

    // Heuristic 2:
    // Try to force associated types of traits to implement debug via where clauses.
    let where_clauses = build_where_clause_for_associated_types(&ast.generics, fields);
    // Since they appear in the where clause, corresponding generic idents must be ommited as well from where clauses.
    for clause in &where_clauses {
        omits.push(clause.1.clone());
    }

    // Provide an escape hatch for cases where it is not possible to apply an heuristic.
    // This hatch should be a macro atribute like: `#[debug(bound = "T::Value: Debug")]`
    let mut attrs = ast.attrs;
    let mut escape_hatch = (false, quote! {});
    if let Some(attr) = attrs.pop() {
        let literal = get_attrs_literal(attr);
        match literal {
            Err(e) => return e.to_compile_error().into(),
            Ok(literal) => {
                let parsed = syn::parse_str::<syn::WherePredicate>(&literal).unwrap();
                escape_hatch = (
                    true,
                    quote! {
                        where #parsed
                    },
                )
            }
        }
    }

    // Generic types bounds
    // If an escape hatch is provided, no bounds are set.
    // As the last test suggests, it would be an improvement to provide the hatch at attribute level too.
    let mut generics = ast.generics;
    if !escape_hatch.0 {
        generics = add_trait_bounds(generics, &omits);
    }
    let (impl_generics, ty_generics, _) = generics.split_for_impl();

    // Prepare where clauses.
    // This takes in account the heuristic 2 (associated types for traits) and the scape hatch, giving the latest most priority.
    let mut wc = quote! {};
    let where_clauses: Vec<_> = where_clauses.iter().map(|c| c.0.clone()).collect();
    if escape_hatch.0 {
        wc = escape_hatch.1;
    } else if !where_clauses.is_empty() {
        wc = quote! {
            where
                #(#where_clauses)*
        };
    }

    // Get calls to the fields in the debug_struct function chain
    let debug_calls = debug_calls(&fields);
    let debug_calls = match debug_calls {
        Ok(calls) => calls,
        Err(e) => return e.to_compile_error().into(),
    };

    quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #wc  {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#struct_name)
                 #(#debug_calls)*
                 .finish()
            }
        }
    }
    .into()
}

/// Get calls to the fields in the debug_struct function chain
fn debug_calls(
    fields: &Punctuated<Field, Comma>,
) -> Result<Vec<proc_macro2::TokenStream>, syn::Error> {
    let debug_calls: Result<Vec<_>, syn::Error> = fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            let name = format!("{}", &ident.as_ref().unwrap());
            let attrs = &f.attrs;

            // If the field has an atribute of the form #[debug = [format]]
            if attrs.len() > 0 {
                let attr = &attrs[0];
                return attr_debug_call(attr, name, ident);
            } else {
                Ok(quote! {
                    .field(#name, &self.#ident)
                })
            }
        })
        .collect();
    debug_calls
}

/// Get debug call for a field with a formatter atribute.
///
/// For example in the bitmask field
///
/// ```
/// use derive_debug::CustomDebug;
/// #[derive(CustomDebug)]
/// pub struct Field {
///     name: &'static str,
///     #[debug = "0b{:08b}"]
///     bitmask: u8,
/// }
///
fn attr_debug_call(
    attr: &syn::Attribute,
    name: String,
    ident: &Option<syn::Ident>,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    let (_, value) = match &attr.meta.require_name_value() {
        Ok(n) => match &n.value {
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(ref lit_str),
                ..
            }) => (n.path.get_ident().unwrap(), lit_str.value()),
            _ => {
                return Err(syn::Error::new_spanned(
                    &n.value,
                    format!("Expected a string formater, for example: {}", {
                        "\"0b{:08b}\""
                    }),
                ));
            }
        },
        Err(_) => {
            return Err(syn::Error::new_spanned(attr, "Invalid attribute format"));
        }
    };
    Ok(quote! {
        .field(#name, &format_args!(#value, &self.#ident))
    })
}

/// Extracts the attribute's literal in the escape hatch attribute.
fn get_attrs_literal(attr: syn::Attribute) -> Result<String, syn::Error> {
    let list = attr.meta.require_list();
    match list {
        Err(_) => return Err(syn::Error::new_spanned(attr, "Invalid argument")),
        Ok(l) => {
            let mut tokens = l.tokens.clone().into_iter();

            // Check that the first token is an Ident with the value "bound"
            if let Some(proc_macro2::TokenTree::Ident(ident)) = tokens.next() {
                if ident != "bound" {
                    return Err(syn::Error::new_spanned(ident, "Invalid ident"));
                }
            } else {
                return Err(syn::Error::new_spanned(&l, "Invalid argument"));
            }

            // Check that the second token is a Punct with the character '='
            if let Some(proc_macro2::TokenTree::Punct(punct)) = tokens.next() {
                if punct.as_char() != '=' {
                    return Err(syn::Error::new_spanned(
                        punct,
                        "Invalid punct, it should be '='",
                    ));
                }
            } else {
                return Err(syn::Error::new_spanned(&l, "Invalid argument"));
            }

            // Check that the third token is a Literal and extract its value
            if let Some(proc_macro2::TokenTree::Literal(literal)) = tokens.next() {
                let stripped_literal = literal.to_string().trim_matches('"').to_owned();
                return Ok(stripped_literal);
            } else {
                return Err(syn::Error::new_spanned(&l, "Invalid argument"));
            }
        }
    }
}

fn build_where_clause_for_associated_types(
    generics: &Generics,
    fields: &Punctuated<Field, Comma>,
) -> Vec<(proc_macro2::TokenStream, syn::Ident)> {
    let mut generic_params_idents = Vec::new();
    for g in &generics.params {
        if let syn::GenericParam::Type(syn::TypeParam { ref ident, .. }) = g {
            generic_params_idents.push(ident);
        }
    }
    if generic_params_idents.is_empty() {
        return vec![];
    }
    let mut res = vec![];
    let generic_strings = &generic_params_idents
        .iter()
        .map(|e| e.to_string())
        .collect();
    for f in fields {
        if let syn::Type::Path(syn::TypePath { ref path, .. }) = f.ty {
            recurse_type_path(&path, &mut res, &generic_strings);
        }
    }

    return res;
}

fn recurse_type_path(
    path: &syn::Path,
    res: &mut Vec<(proc_macro2::TokenStream, syn::Ident)>,
    generics: &Vec<String>,
) {
    if let Some(segment) = path.segments.first() {
        if generics.contains(&segment.ident.to_string()) && path.segments.len() > 1 {
            res.push((
                quote! {
                    #path: std::fmt::Debug,
                },
                segment.ident.clone(),
            ));
        } else if let PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
            ref args,
            ..
        }) = &segment.arguments
        {
            for arg in args {
                if let syn::GenericArgument::Type(syn::Type::Path(syn::TypePath { path, .. })) = arg
                {
                    recurse_type_path(path, res, generics);
                }
            }
        }
    }
}

fn identify_phantom_data_generics(fields: &Punctuated<Field, Comma>) -> Vec<syn::Ident> {
    let mut res = vec![];
    for field in fields {
        if let syn::Type::Path(ref p) = field.ty {
            for segment in &p.path.segments {
                if segment.ident == "PhantomData" {
                    let gen = find_generic_idents(&segment);
                    for i in gen {
                        res.push(i.clone());
                    }
                }
            }
        }
    }
    res
}

fn find_generic_idents<'a>(segment: &'a PathSegment) -> Vec<&'a syn::Ident> {
    let mut res = vec![];
    if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
        args, ..
    }) = &segment.arguments
    {
        for arg in args {
            if let syn::GenericArgument::Type(syn::Type::Path(ref segments, ..)) = arg {
                for s in &segments.path.segments {
                    res.push(&s.ident);
                }
            }
        }
    }
    res
}

/// Add trait bound Debug for types not in omits
fn add_trait_bounds(mut generics: Generics, omits: &Vec<syn::Ident>) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            if !omits.contains(&type_param.ident) {
                type_param.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    }
    generics
}
