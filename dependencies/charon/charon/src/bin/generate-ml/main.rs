//! Generate ocaml deserialization code for our types.
//!
//! This binary runs charon on itself and generates the appropriate `<type>_of_json` functions for
//! our types. The generated functions are inserted into `./generate-ml/GAstOfJson.template.ml` to
//! construct the final `GAstOfJson.ml`.
//!
//! To run it, call `cargo run --bin generate-ml`. It is also run by `make generate-ml` in the
//! crate root. Don't forget to format the output code after regenerating.
#![feature(if_let_guard)]

use anyhow::{Context, Result, bail};
use assert_cmd::cargo::CommandCargoExt;
use charon_lib::ast::*;
use convert_case::{Case, Casing};
use indoc::indoc;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

/// `Name` is a complex datastructure; to inspect it we serialize it a little bit.
fn repr_name(_crate_data: &TranslatedCrate, n: &Name) -> String {
    n.name
        .iter()
        .map(|path_elem| match path_elem {
            PathElem::Ident(i, _) => i.clone(),
            PathElem::Impl(..) => "<impl>".to_string(),
            PathElem::Monomorphized(..) => "<mono>".to_string(),
        })
        .join("::")
}

fn make_ocaml_ident(name: &str) -> String {
    let mut name = name.to_case(Case::Snake);
    if matches!(
        &*name,
        "virtual"
            | "bool"
            | "char"
            | "struct"
            | "type"
            | "let"
            | "fun"
            | "open"
            | "rec"
            | "assert"
            | "float"
            | "end"
            | "include"
            | "to"
    ) {
        name += "_";
    }
    name
}
fn type_name_to_ocaml_ident(item_meta: &ItemMeta) -> String {
    let name = item_meta
        .attr_info
        .rename
        .as_ref()
        .unwrap_or(item_meta.name.name.last().unwrap().as_ident().unwrap().0);
    make_ocaml_ident(name)
}

struct GenerateCtx<'a> {
    crate_data: &'a TranslatedCrate,
    name_to_type: HashMap<String, &'a TypeDecl>,
    /// For each type, list the types it contains.
    type_tree: HashMap<TypeDeclId, HashSet<TypeDeclId>>,
    manual_type_impls: HashMap<TypeDeclId, String>,
    manual_json_impls: HashMap<TypeDeclId, String>,
    opaque_for_visitor: HashSet<TypeDeclId>,
}

impl<'a> GenerateCtx<'a> {
    fn new(
        crate_data: &'a TranslatedCrate,
        manual_type_impls: &[(&str, &str)],
        manual_json_impls: &[(&str, &str)],
        opaque_for_visitor: &[&str],
    ) -> Self {
        let mut name_to_type: HashMap<String, &TypeDecl> = Default::default();
        let mut type_tree = HashMap::default();
        for ty in &crate_data.type_decls {
            let long_name = repr_name(crate_data, &ty.item_meta.name);
            if long_name.starts_with("charon_lib") {
                let short_name = ty
                    .item_meta
                    .name
                    .name
                    .last()
                    .unwrap()
                    .as_ident()
                    .unwrap()
                    .0
                    .clone();
                name_to_type.insert(short_name, ty);
            }
            name_to_type.insert(long_name, ty);

            let mut contained = HashSet::new();
            ty.dyn_visit(|id: &TypeDeclId| {
                contained.insert(*id);
            });
            type_tree.insert(ty.def_id, contained);
        }

        let mut ctx = GenerateCtx {
            crate_data: &crate_data,
            name_to_type,
            type_tree,
            manual_type_impls: Default::default(),
            manual_json_impls: Default::default(),
            opaque_for_visitor: Default::default(),
        };
        ctx.manual_type_impls = manual_type_impls
            .iter()
            .map(|(name, def)| (ctx.id_from_name(name), def.to_string()))
            .collect();
        ctx.manual_json_impls = manual_json_impls
            .iter()
            .map(|(name, def)| (ctx.id_from_name(name), def.to_string()))
            .collect();
        ctx.opaque_for_visitor = opaque_for_visitor
            .iter()
            .map(|name| ctx.id_from_name(name))
            .collect();
        ctx
    }

    fn id_from_name(&self, name: &str) -> TypeDeclId {
        self.name_to_type
            .get(name)
            .expect(&format!("Name not found: `{name}`"))
            .def_id
    }

    /// List the (recursive) children of this type.
    fn children_of(&self, name: &str) -> HashSet<TypeDeclId> {
        let start_id = self.id_from_name(name);
        self.children_of_inner(vec![start_id])
    }

    /// List the (recursive) children of these types.
    fn children_of_many(&self, names: &[&str]) -> HashSet<TypeDeclId> {
        self.children_of_inner(names.iter().map(|name| self.id_from_name(name)).collect())
    }

    fn children_of_inner(&self, ty: Vec<TypeDeclId>) -> HashSet<TypeDeclId> {
        let mut children = HashSet::new();
        let mut stack = ty.to_vec();
        while let Some(id) = stack.pop() {
            if !children.contains(&id)
                && self
                    .crate_data
                    .type_decls
                    .get(id)
                    .is_some_and(|decl| decl.item_meta.is_local)
            {
                children.insert(id);
                if let Some(contained) = self.type_tree.get(&id) {
                    stack.extend(contained);
                }
            }
        }
        children
    }
}

/// Converts a type to the appropriate `*_of_json` call. In case of generics, this combines several
/// functions, e.g. `list_of_json bool_of_json`.
fn type_to_ocaml_call(ctx: &GenerateCtx, ty: &Ty) -> String {
    match ty.kind() {
        TyKind::Literal(LiteralTy::Bool) => "bool_of_json".to_string(),
        TyKind::Literal(LiteralTy::Char) => "char_of_json".to_string(),
        TyKind::Literal(LiteralTy::Int(int_ty)) => match int_ty {
            // Even though OCaml ints are only 63 bits, only scalars with their 128 bits should be able to become too large
            IntTy::I128 => "big_int_of_json".to_string(),
            _ => "int_of_json".to_string(),
        },
        TyKind::Literal(LiteralTy::UInt(uint_ty)) => match uint_ty {
            // Even though OCaml ints are only 63 bits, only scalars with their 128 bits should be able to become too large
            UIntTy::U128 => "big_int_of_json".to_string(),
            _ => "int_of_json".to_string(),
        },
        TyKind::Literal(LiteralTy::Float(_)) => "float_of_json".to_string(),
        TyKind::Adt(tref) => {
            let mut expr = Vec::new();
            for ty in &tref.generics.types {
                expr.push(type_to_ocaml_call(ctx, ty))
            }
            match tref.id {
                TypeId::Adt(id) => {
                    let mut first = if let Some(tdecl) = ctx.crate_data.type_decls.get(id) {
                        type_name_to_ocaml_ident(&tdecl.item_meta)
                    } else {
                        format!("missing_type_{id}")
                    };
                    if first == "vec" {
                        first = "list".to_string();
                    }
                    expr.insert(0, first + "_of_json");
                }
                TypeId::Builtin(BuiltinTy::Box) => expr.insert(0, "box_of_json".to_owned()),
                TypeId::Tuple => {
                    let name = match tref.generics.types.elem_count() {
                        2 => "pair_of_json".to_string(),
                        3 => "triple_of_json".to_string(),
                        len => format!("tuple_{len}_of_json"),
                    };
                    expr.insert(0, name);
                }
                _ => unimplemented!("{ty:?}"),
            }
            expr.into_iter().map(|f| format!("({f})")).join(" ")
        }
        TyKind::TypeVar(DeBruijnVar::Free(id)) => format!("arg{id}_of_json"),
        _ => unimplemented!("{ty:?}"),
    }
}

/// Converts a type to the appropriate ocaml name. In case of generics, this provides appropriate
/// parameters.
fn type_to_ocaml_name(ctx: &GenerateCtx, ty: &Ty) -> String {
    match ty.kind() {
        TyKind::Literal(LiteralTy::Bool) => "bool".to_string(),
        TyKind::Literal(LiteralTy::Char) => "(Uchar.t [@visitors.opaque])".to_string(),
        TyKind::Literal(LiteralTy::Int(int_ty)) => match int_ty {
            // Even though OCaml ints are only 63 bits, only scalars with their 128 bits should be able to become too large
            IntTy::I128 => "big_int".to_string(),
            _ => "int".to_string(),
        },
        TyKind::Literal(LiteralTy::UInt(uint_ty)) => match uint_ty {
            // Even though OCaml ints are only 63 bits, only scalars with their 128 bits should be able to become too large
            UIntTy::U128 => "big_int".to_string(),
            _ => "int".to_string(),
        },
        TyKind::Literal(LiteralTy::Float(_)) => "float_of_json".to_string(),
        TyKind::Adt(tref) => {
            let mut args = tref
                .generics
                .types
                .iter()
                .map(|ty| type_to_ocaml_name(ctx, ty))
                .map(|name| {
                    if !name.chars().all(|c| c.is_alphanumeric()) {
                        format!("({name})")
                    } else {
                        name
                    }
                })
                .collect_vec();
            match tref.id {
                TypeId::Adt(id) => {
                    let mut base_ty = if let Some(tdecl) = ctx.crate_data.type_decls.get(id) {
                        type_name_to_ocaml_ident(&tdecl.item_meta)
                    } else if let Some(name) = ctx.crate_data.item_name(id) {
                        eprintln!(
                            "Warning: type {} missing from llbc",
                            repr_name(ctx.crate_data, name)
                        );
                        name.name
                            .last()
                            .unwrap()
                            .as_ident()
                            .unwrap()
                            .0
                            .to_lowercase()
                    } else {
                        format!("missing_type_{id}")
                    };
                    if base_ty == "vec" {
                        base_ty = "list".to_string();
                    }
                    if base_ty == "vector" {
                        base_ty = "list".to_string();
                        args.remove(0); // Remove the index generic param
                    }
                    let args = match args.as_slice() {
                        [] => String::new(),
                        [arg] => arg.clone(),
                        args => format!("({})", args.iter().join(",")),
                    };
                    format!("{args} {base_ty}")
                }
                TypeId::Builtin(BuiltinTy::Box) => args[0].clone(),
                TypeId::Tuple => args.iter().join("*"),
                _ => unimplemented!("{ty:?}"),
            }
        }
        TyKind::TypeVar(DeBruijnVar::Free(id)) => format!("'a{id}"),
        _ => unimplemented!("{ty:?}"),
    }
}

fn convert_vars<'a>(ctx: &GenerateCtx, fields: impl IntoIterator<Item = &'a Field>) -> String {
    fields
        .into_iter()
        .filter(|f| !f.is_opaque())
        .map(|f| {
            let name = make_ocaml_ident(f.name.as_deref().unwrap());
            let rename = make_ocaml_ident(f.renamed_name().unwrap());
            let convert = type_to_ocaml_call(ctx, &f.ty);
            format!("let* {rename} = {convert} ctx {name} in")
        })
        .join("\n")
}

fn build_branch<'a>(
    ctx: &GenerateCtx,
    pat: &str,
    fields: impl IntoIterator<Item = &'a Field>,
    construct: &str,
) -> String {
    let convert = convert_vars(ctx, fields);
    format!("| {pat} -> {convert} Ok ({construct})")
}

fn build_function(_ctx: &GenerateCtx, decl: &TypeDecl, branches: &str) -> String {
    let ty_name = type_name_to_ocaml_ident(&decl.item_meta);
    let signature = if decl.generics.types.is_empty() {
        format!("{ty_name}_of_json (ctx : of_json_ctx) (js : json) : ({ty_name}, string) result =")
    } else {
        let types = &decl.generics.types;
        let gen_vars_space = types
            .iter()
            .enumerate()
            .map(|(i, _)| format!("'a{i}"))
            .join(" ");
        let gen_vars_comma = types
            .iter()
            .enumerate()
            .map(|(i, _)| format!("'a{i}"))
            .join(", ");

        let mut args = Vec::new();
        let mut ty_args = Vec::new();
        for (i, _) in types.iter().enumerate() {
            args.push(format!("arg{i}_of_json"));
            ty_args.push(format!("(of_json_ctx -> json -> ('a{i}, string) result)"));
        }
        args.push("ctx".to_string());
        ty_args.push("of_json_ctx".to_string());
        args.push("js".to_string());
        ty_args.push("json".to_string());

        let ty_args = ty_args.into_iter().join(" -> ");
        let args = args.into_iter().join(" ");
        let fun_ty =
            format!("{gen_vars_space}. {ty_args} -> (({gen_vars_comma}) {ty_name}, string) result");
        format!("{ty_name}_of_json : {fun_ty} = fun {args} ->")
    };
    format!(
        r#"
        and {signature}
          combine_error_msgs js __FUNCTION__
            (match js with{branches} | _ -> Error "")
        "#
    )
}

fn type_decl_to_json_deserializer(ctx: &GenerateCtx, decl: &TypeDecl) -> String {
    let return_ty = type_name_to_ocaml_ident(&decl.item_meta);
    let return_ty = if decl.generics.types.is_empty() {
        return_ty
    } else {
        format!("_ {return_ty}")
    };

    let branches = match &decl.kind {
        _ if let Some(def) = ctx.manual_json_impls.get(&decl.def_id) => def.clone(),
        TypeDeclKind::Struct(fields) if fields.is_empty() => {
            build_branch(ctx, "`Null", fields, "()")
        }
        TypeDeclKind::Struct(fields)
            if fields.elem_count() == 1
                && fields[0].name.as_ref().is_some_and(|name| name == "_raw") =>
        {
            // These are the special strongly-typed integers.
            let short_name = decl
                .item_meta
                .name
                .name
                .last()
                .unwrap()
                .as_ident()
                .unwrap()
                .0
                .clone();
            format!("| x -> {short_name}.id_of_json ctx x")
        }
        TypeDeclKind::Struct(fields)
            if fields.elem_count() == 1
                && (fields[0].name.is_none()
                    || decl
                        .item_meta
                        .attr_info
                        .attributes
                        .iter()
                        .filter_map(|a| a.as_unknown())
                        .any(|a| a.to_string() == "serde(transparent)")) =>
        {
            let ty = &fields[0].ty;
            let call = type_to_ocaml_call(ctx, ty);
            format!("| x -> {call} ctx x")
        }
        TypeDeclKind::Alias(ty) => {
            let call = type_to_ocaml_call(ctx, ty);
            format!("| x -> {call} ctx x")
        }
        TypeDeclKind::Struct(fields) if fields.iter().all(|f| f.name.is_none()) => {
            let mut fields = fields.clone();
            for (i, f) in fields.iter_mut().enumerate() {
                f.name = Some(format!("x{i}"));
            }
            let pat: String = fields
                .iter()
                .map(|f| f.name.as_deref().unwrap())
                .map(|n| make_ocaml_ident(n))
                .join(";");
            let pat = format!("`List [ {pat} ]");
            let construct = fields
                .iter()
                .map(|f| f.renamed_name().unwrap())
                .map(|n| make_ocaml_ident(n))
                .join(", ");
            let construct = format!("( {construct} )");
            build_branch(ctx, &pat, &fields, &construct)
        }
        TypeDeclKind::Struct(fields) => {
            let fields = fields
                .iter()
                .filter(|field| {
                    !field
                        .attr_info
                        .attributes
                        .iter()
                        .filter_map(|a| a.as_unknown())
                        .any(|a| a.to_string() == "serde(skip)")
                })
                .collect_vec();
            let pat: String = fields
                .iter()
                .map(|f| {
                    let name = f.name.as_ref().unwrap();
                    let var = if f.is_opaque() {
                        "_"
                    } else {
                        &make_ocaml_ident(name)
                    };
                    format!("(\"{name}\", {var});")
                })
                .join("\n");
            let pat = format!("`Assoc [ {pat} ]");
            let construct = fields
                .iter()
                .filter(|f| !f.is_opaque())
                .map(|f| f.renamed_name().unwrap())
                .map(|n| make_ocaml_ident(n))
                .join("; ");
            let construct = format!("({{ {construct} }} : {return_ty})");
            build_branch(ctx, &pat, fields, &construct)
        }
        TypeDeclKind::Enum(variants) => {
            variants
                .iter()
                .filter(|v| !v.is_opaque())
                .map(|variant| {
                    let name = &variant.name;
                    let rename = variant.renamed_name();
                    if variant.fields.is_empty() {
                        // Unit variant
                        let pat = format!("`String \"{name}\"");
                        build_branch(ctx, &pat, &variant.fields, rename)
                    } else {
                        let mut fields = variant.fields.clone();
                        let inner_pat = if fields.iter().all(|f| f.name.is_none()) {
                            // Tuple variant
                            if variant.fields.elem_count() == 1 {
                                let var = make_ocaml_ident(&variant.name);
                                fields[0].name = Some(var.clone());
                                var
                            } else {
                                for (i, f) in fields.iter_mut().enumerate() {
                                    f.name = Some(format!("x_{i}"));
                                }
                                let pat =
                                    fields.iter().map(|f| f.name.as_ref().unwrap()).join("; ");
                                format!("`List [ {pat} ]")
                            }
                        } else {
                            // Struct variant
                            let pat = fields
                                .iter()
                                .map(|f| {
                                    let name = f.name.as_ref().unwrap();
                                    let var = if f.is_opaque() {
                                        "_"
                                    } else {
                                        &make_ocaml_ident(name)
                                    };
                                    format!("(\"{name}\", {var});")
                                })
                                .join(" ");
                            format!("`Assoc [ {pat} ]")
                        };
                        let pat = format!("`Assoc [ (\"{name}\", {inner_pat}) ]");
                        let construct_fields = fields
                            .iter()
                            .map(|f| f.name.as_ref().unwrap())
                            .map(|n| make_ocaml_ident(n))
                            .join(", ");
                        let construct = format!("{rename} ({construct_fields})");
                        build_branch(ctx, &pat, &fields, &construct)
                    }
                })
                .join("\n")
        }
        TypeDeclKind::Union(..) => todo!(),
        TypeDeclKind::Opaque => todo!(),
        TypeDeclKind::Error(_) => todo!(),
    };
    build_function(ctx, decl, &branches)
}

fn extract_doc_comments(attr_info: &AttrInfo) -> String {
    attr_info
        .attributes
        .iter()
        .filter_map(|a| a.as_doc_comment())
        .join("\n")
}

/// Make a doc comment that contains the given string, indenting it if necessary.
fn build_doc_comment(comment: String, indent_level: usize) -> String {
    #[derive(Default)]
    struct Exchanger {
        is_in_open_escaped_block: bool,
        is_in_open_inline_escape: bool,
    }
    impl Exchanger {
        pub fn exchange_escape_delimiters(&mut self, line: &str) -> String {
            if line.contains("```") {
                // Handle multi-line escaped blocks.
                let (leading, mut rest) = line.split_once("```").unwrap();

                // Strip all (hard-coded) possible ways to open the block.
                rest = if rest.starts_with("text") {
                    rest.strip_prefix("text").unwrap()
                } else if rest.starts_with("rust,ignore") {
                    rest.strip_prefix("rust,ignore").unwrap()
                } else {
                    rest
                };
                let mut result = leading.to_owned();
                if self.is_in_open_escaped_block {
                    result.push_str("]}");
                    self.is_in_open_escaped_block = false;
                } else {
                    result.push_str("{@rust[");
                    self.is_in_open_escaped_block = true;
                }
                result.push_str(rest);
                result
            } else if line.contains('`') {
                // Handle inline escaped strings. These can occur in multiple lines, so we track them globally.
                let mut parts = line.split('`');
                // Skip after first part (we only need to add escapes after that).
                let mut result = parts.next().unwrap().to_owned();
                for part in parts {
                    if self.is_in_open_inline_escape {
                        result.push_str("]");
                        result.push_str(part);
                        self.is_in_open_inline_escape = false;
                    } else {
                        result.push_str("[");
                        result.push_str(part);
                        self.is_in_open_inline_escape = true;
                    }
                }
                result
            } else {
                line.to_owned()
            }
        }
    }

    if comment == "" {
        return comment;
    }
    let is_multiline = comment.contains("\n");
    if !is_multiline {
        let fixed_comment = Exchanger::default().exchange_escape_delimiters(&comment);
        format!("(**{fixed_comment} *)")
    } else {
        let indent = "  ".repeat(indent_level);
        let mut exchanger = Exchanger::default();
        let comment = comment
            .lines()
            .enumerate()
            .map(|(i, line)| {
                // Remove one leading space if there is one (there usually is because we write `///
                // comment` and not `///comment`).
                let line = line.strip_prefix(" ").unwrap_or(line);
                let fixed_line = exchanger.exchange_escape_delimiters(line);

                // The first line follows the `(**` marker, it does not need to be indented.
                // Neither do empty lines.
                if i == 0 || fixed_line.is_empty() {
                    fixed_line
                } else {
                    format!("{indent}    {fixed_line}")
                }
            })
            .join("\n");
        format!("(** {comment}\n{indent} *)")
    }
}

fn build_type(_ctx: &GenerateCtx, decl: &TypeDecl, co_rec: bool, body: &str) -> String {
    let ty_name = type_name_to_ocaml_ident(&decl.item_meta);
    let generics = decl
        .generics
        .types
        .iter()
        .enumerate()
        .map(|(i, _)| format!("'a{i}"))
        .collect_vec();
    let generics = match generics.as_slice() {
        [] => String::new(),
        [ty] => ty.clone(),
        generics => format!("({})", generics.iter().join(",")),
    };
    let comment = extract_doc_comments(&decl.item_meta.attr_info);
    let comment = build_doc_comment(comment, 0);
    let keyword = if co_rec { "and" } else { "type" };
    format!("\n{comment} {keyword} {generics} {ty_name} = {body}")
}

/// Generate an ocaml type declaration that mirrors `decl`.
///
/// `co_rec` indicates whether this definition is co-recursive with the ones that come before (i.e.
/// should be declared with `and` instead of `type`).
fn type_decl_to_ocaml_decl(ctx: &GenerateCtx, decl: &TypeDecl, co_rec: bool) -> String {
    let opaque = if ctx.opaque_for_visitor.contains(&decl.def_id) {
        "[@visitors.opaque]"
    } else {
        ""
    };
    let body = match &decl.kind {
        _ if let Some(def) = ctx.manual_type_impls.get(&decl.def_id) => def.clone(),
        TypeDeclKind::Alias(ty) => {
            let ty = type_to_ocaml_name(ctx, ty);
            format!("{ty} {opaque}")
        }
        TypeDeclKind::Struct(fields) if fields.is_empty() => "unit".to_string(),
        TypeDeclKind::Struct(fields)
            if fields.elem_count() == 1
                && fields[0].name.as_ref().is_some_and(|name| name == "_raw") =>
        {
            // These are the special strongly-typed integers.
            let short_name = decl
                .item_meta
                .name
                .name
                .last()
                .unwrap()
                .as_ident()
                .unwrap()
                .0
                .clone();
            format!("{short_name}.id [@visitors.opaque]")
        }
        TypeDeclKind::Struct(fields)
            if fields.elem_count() == 1
                && (fields[0].name.is_none()
                    || decl
                        .item_meta
                        .attr_info
                        .attributes
                        .iter()
                        .filter_map(|a| a.as_unknown())
                        .any(|a| a.to_string() == "serde(transparent)")) =>
        {
            let ty = type_to_ocaml_name(ctx, &fields[0].ty);
            format!("{ty} {opaque}")
        }
        TypeDeclKind::Struct(fields) if fields.iter().all(|f| f.name.is_none()) => fields
            .iter()
            .filter(|f| !f.is_opaque())
            .map(|f| {
                let ty = type_to_ocaml_name(ctx, &f.ty);
                format!("{ty} {opaque}")
            })
            .join("*"),
        TypeDeclKind::Struct(fields) => {
            let fields = fields
                .iter()
                .filter(|f| !f.is_opaque())
                .map(|f| {
                    let name = f.renamed_name().unwrap();
                    let ty = type_to_ocaml_name(ctx, &f.ty);
                    let comment = extract_doc_comments(&f.attr_info);
                    let comment = build_doc_comment(comment, 2);
                    format!("{name} : {ty} {opaque} {comment}")
                })
                .join(";");
            format!("{{ {fields} }}")
        }
        TypeDeclKind::Enum(variants) => {
            variants
                .iter()
                .filter(|v| !v.is_opaque())
                .map(|variant| {
                    let mut attr_info = variant.attr_info.clone();
                    let rename = variant.renamed_name();
                    let ty = if variant.fields.is_empty() {
                        // Unit variant
                        String::new()
                    } else {
                        if variant.fields.iter().all(|f| f.name.is_some()) {
                            let fields = variant
                                .fields
                                .iter()
                                .map(|f| {
                                    let comment = extract_doc_comments(&f.attr_info);
                                    let description = if comment.is_empty() {
                                        comment
                                    } else {
                                        format!(": {comment}")
                                    };
                                    format!("\n - [{}]{description}", f.name.as_ref().unwrap())
                                })
                                .join("");
                            let field_descriptions = format!("\n Fields:{fields}");
                            // Add a constructed doc-comment
                            attr_info
                                .attributes
                                .push(Attribute::DocComment(field_descriptions));
                        }
                        let fields = variant
                            .fields
                            .iter()
                            .map(|f| {
                                let ty = type_to_ocaml_name(ctx, &f.ty);
                                format!("{ty} {opaque}")
                            })
                            .join("*");
                        format!(" of {fields}")
                    };
                    let comment = extract_doc_comments(&attr_info);
                    let comment = build_doc_comment(comment, 3);
                    format!("\n\n | {rename}{ty} {comment}")
                })
                .join("")
        }
        TypeDeclKind::Union(..) => todo!(),
        TypeDeclKind::Opaque => todo!(),
        TypeDeclKind::Error(_) => todo!(),
    };
    build_type(ctx, decl, co_rec, &body)
}

fn generate_visitor_bases(
    _ctx: &GenerateCtx,
    name: &str,
    inherits: &[&str],
    reduce: bool,
    ty_names: &[String],
) -> String {
    let mut out = String::new();
    let make_inherit = |variety| {
        if !inherits.is_empty() {
            inherits
                .iter()
                .map(|ancestor| {
                    if let [module, name] = ancestor.split(".").collect_vec().as_slice() {
                        format!("inherit [_] {module}.{variety}_{name}")
                    } else {
                        format!("inherit [_] {variety}_{ancestor}")
                    }
                })
                .join("\n")
        } else {
            format!("inherit [_] VisitorsRuntime.{variety}")
        }
    };

    let iter_methods = ty_names
        .iter()
        .map(|ty| format!("method visit_{ty} : 'env -> {ty} -> unit = fun _ _ -> ()"))
        .format("\n");
    let _ = write!(
        &mut out,
        "
        class ['self] iter_{name} =
          object (self : 'self)
            {}
            {iter_methods}
          end
        ",
        make_inherit("iter")
    );

    let map_methods = ty_names
        .iter()
        .map(|ty| format!("method visit_{ty} : 'env -> {ty} -> {ty} = fun _ x -> x"))
        .format("\n");
    let _ = write!(
        &mut out,
        "
        class ['self] map_{name} =
          object (self : 'self)
            {}
            {map_methods}
          end
        ",
        make_inherit("map")
    );

    if reduce {
        let reduce_methods = ty_names
            .iter()
            .map(|ty| format!("method visit_{ty} : 'env -> {ty} -> 'a = fun _ _ -> self#zero"))
            .format("\n");
        let _ = write!(
            &mut out,
            "
            class virtual ['self] reduce_{name} =
              object (self : 'self)
                {}
                {reduce_methods}
              end
            ",
            make_inherit("reduce")
        );

        let mapreduce_methods = ty_names
            .iter()
            .map(|ty| {
                format!("method visit_{ty} : 'env -> {ty} -> {ty} * 'a = fun _ x -> (x, self#zero)")
            })
            .format("\n");
        let _ = write!(
            &mut out,
            "
            class virtual ['self] mapreduce_{name} =
              object (self : 'self)
                {}
                {mapreduce_methods}
              end
            ",
            make_inherit("mapreduce")
        );
    }

    out
}

#[derive(Clone, Copy)]
struct DeriveVisitors {
    name: &'static str,
    ancestors: &'static [&'static str],
    reduce: bool,
    extra_types: &'static [&'static str],
}

/// The kind of code generation to perform.
#[derive(Clone, Copy)]
enum GenerationKind {
    OfJson,
    TypeDecl(Option<DeriveVisitors>),
}

/// Replace markers in `template` with auto-generated code.
struct GenerateCodeFor {
    template: PathBuf,
    target: PathBuf,
    /// Each list corresponds to a marker. We replace the ith `__REPLACE{i}__` marker with
    /// generated code for each definition in the ith list.
    ///
    /// Eventually we should reorder definitions so the generated ones are all in one block.
    /// Keeping the order is important while we migrate away from hand-written code.
    markers: Vec<(GenerationKind, HashSet<TypeDeclId>)>,
}

impl GenerateCodeFor {
    fn generate(&self, ctx: &GenerateCtx) -> Result<()> {
        let mut template = fs::read_to_string(&self.template)
            .with_context(|| format!("Failed to read template file {}", self.template.display()))?;
        for (i, (kind, names)) in self.markers.iter().enumerate() {
            let tys = names
                .iter()
                .map(|&id| &ctx.crate_data[id])
                .sorted_by_key(|tdecl| {
                    tdecl
                        .item_meta
                        .name
                        .name
                        .last()
                        .unwrap()
                        .as_ident()
                        .unwrap()
                });
            let generated = match kind {
                GenerationKind::OfJson => {
                    let fns = tys
                        .map(|ty| type_decl_to_json_deserializer(ctx, ty))
                        .format("\n");
                    format!("let rec ___ = ()\n{fns}")
                }
                GenerationKind::TypeDecl(visitors) => {
                    let mut decls = tys
                        .enumerate()
                        .map(|(i, ty)| {
                            let co_recursive = i != 0;
                            type_decl_to_ocaml_decl(ctx, ty, co_recursive)
                        })
                        .join("\n");
                    if let Some(visitors) = visitors {
                        let &DeriveVisitors {
                            name,
                            mut ancestors,
                            reduce,
                            extra_types,
                        } = visitors;
                        let varieties: &[_] = if reduce {
                            &["iter", "map", "reduce", "mapreduce"]
                        } else {
                            &["iter", "map"]
                        };
                        let intermediate_visitor_name;
                        let intermediate_visitor_name_slice;
                        if !extra_types.is_empty() {
                            intermediate_visitor_name = format!("{name}_base");
                            let intermediate_visitor = generate_visitor_bases(
                                ctx,
                                &intermediate_visitor_name,
                                ancestors,
                                reduce,
                                extra_types
                                    .iter()
                                    .map(|s| s.to_string())
                                    .collect_vec()
                                    .as_slice(),
                            );
                            intermediate_visitor_name_slice = [intermediate_visitor_name.as_str()];
                            ancestors = &intermediate_visitor_name_slice;
                            decls = format!(
                                "(* Ancestors for the {name} visitors *){intermediate_visitor}\n{decls}"
                            );
                        }
                        let visitors = varieties
                            .iter()
                            .map(|variety| {
                                let nude = if !ancestors.is_empty() {
                                    format!("nude = true (* Don't inherit VisitorsRuntime *);")
                                } else {
                                    String::new()
                                };
                                let ancestors = format!(
                                    "ancestors = [ {} ];",
                                    ancestors
                                        .iter()
                                        .map(|a| format!("\"{variety}_{a}\""))
                                        .join(";")
                                );
                                format!(
                                    r#"
                                    visitors {{
                                        name = "{variety}_{name}";
                                        monomorphic = ["env"];
                                        variety = "{variety}";
                                        {ancestors}
                                        {nude}
                                    }}
                                "#
                                )
                            })
                            .format(", ");
                        let _ = write!(&mut decls, "\n[@@deriving show, eq, ord, {visitors}]");
                    };
                    decls
                }
            };
            let placeholder = format!("(* __REPLACE{i}__ *)");
            template = template.replace(&placeholder, &generated);
        }

        fs::write(&self.target, template)
            .with_context(|| format!("Failed to write generated file {}", self.target.display()))?;
        Ok(())
    }
}

fn main() -> Result<()> {
    let dir = PathBuf::from("src/bin/generate-ml");
    let charon_llbc = dir.join("charon-itself.ullbc");
    let reuse_llbc = std::env::var("CHARON_ML_REUSE_LLBC").is_ok(); // Useful when developping
    if !reuse_llbc {
        // Call charon on itself
        let mut cmd = Command::cargo_bin("charon")?;
        cmd.arg("cargo");
        cmd.arg("--hide-marker-traits");
        cmd.arg("--hide-allocator");
        cmd.arg("--ullbc");
        cmd.arg("--start-from=charon_lib::ast::krate::TranslatedCrate");
        cmd.arg("--start-from=charon_lib::ast::ullbc_ast::BodyContents");
        cmd.arg("--exclude=charon_lib::common::hash_consing::HashConsed");
        cmd.arg("--dest-file");
        cmd.arg(&charon_llbc);
        cmd.arg("--");
        cmd.arg("--lib");
        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8(output.stderr.clone())?;
            bail!("Compilation failed: {stderr}")
        }
    }

    let crate_data: TranslatedCrate = charon_lib::deserialize_llbc(&charon_llbc)?;
    let output_dir = if std::env::var("IN_CI").as_deref() == Ok("1") {
        dir.join("generated")
    } else {
        dir.join("../../../../charon-ml/src/generated")
    };
    generate_ml(crate_data, dir.join("templates"), output_dir)
}

fn generate_ml(
    crate_data: TranslatedCrate,
    template_dir: PathBuf,
    output_dir: PathBuf,
) -> anyhow::Result<()> {
    let manual_type_impls = &[
        // Hand-written because we replace the `FileId` with the corresponding file.
        ("FileId", "file"),
        // Handwritten because we use `indexed_var` as a hack to be able to reuse field names.
        // TODO: remove the need for this hack.
        ("RegionVar", "(region_id, string option) indexed_var"),
        ("TypeVar", "(type_var_id, string) indexed_var"),
    ];
    let manual_json_impls = &[
        // Hand-written because we filter out `None` values.
        (
            "Vector",
            indoc!(
                r#"
                | js ->
                    let* list = list_of_json (option_of_json arg1_of_json) ctx js in
                    Ok (List.filter_map (fun x -> x) list)
                "#
            ),
        ),
        // Hand-written because we replace the `FileId` with the corresponding file name.
        (
            "FileId",
            indoc!(
                r#"
                | json ->
                    let* file_id = FileId.id_of_json ctx json in
                    let file = FileId.Map.find file_id ctx in
                    Ok file
                "#,
            ),
        ),
    ];
    // Types for which we don't want to generate a type at all.
    let dont_generate_ty = &[
        "ItemOpacity",
        "PredicateOrigin",
        "TraitTypeConstraintId",
        "Ty",
        "Vector",
    ];
    // Types that we don't want visitors to go into.
    let opaque_for_visitor = &["Name"];
    let ctx = GenerateCtx::new(
        &crate_data,
        manual_type_impls,
        manual_json_impls,
        opaque_for_visitor,
    );

    // Compute the sets of types to be put in each module.
    let manually_implemented: HashSet<_> = [
        "ItemOpacity",
        "PredicateOrigin",
        "Ty", // We exclude it since `TyKind` is renamed to `ty`
        "Opaque",
        "Body",
        "FunDecl",
        "TranslatedCrate",
    ]
    .iter()
    .map(|name| ctx.id_from_name(name))
    .collect();

    // Compute type sets for json deserializers.
    let (gast_types, llbc_types, ullbc_types) = {
        let llbc_types: HashSet<_> = ctx.children_of("charon_lib::ast::llbc_ast::Statement");
        let ullbc_types: HashSet<_> = ctx.children_of("charon_lib::ast::ullbc_ast::BodyContents");
        let all_types: HashSet<_> = ctx.children_of("TranslatedCrate");

        let shared_types: HashSet<_> = llbc_types.intersection(&ullbc_types).copied().collect();
        let llbc_types: HashSet<_> = llbc_types.difference(&shared_types).copied().collect();
        let ullbc_types: HashSet<_> = ullbc_types.difference(&shared_types).copied().collect();

        let body_specific_types: HashSet<_> = llbc_types.union(&ullbc_types).copied().collect();
        let gast_types: HashSet<_> = all_types
            .difference(&body_specific_types)
            .copied()
            .collect();

        let gast_types: HashSet<_> = gast_types
            .difference(&manually_implemented)
            .copied()
            .collect();
        let llbc_types: HashSet<_> = llbc_types
            .difference(&manually_implemented)
            .copied()
            .collect();
        let ullbc_types: HashSet<_> = ullbc_types
            .difference(&manually_implemented)
            .copied()
            .collect();
        (gast_types, llbc_types, ullbc_types)
    };

    let mut processed_tys: HashSet<TypeDeclId> = dont_generate_ty
        .iter()
        .map(|name| ctx.id_from_name(name))
        .collect();
    // Each call to this will return the children of the listed types that haven't been returned
    // yet. By calling it in dependency order, this allows to organize types into files without
    // having to list them all.
    let mut markers_from_children = |ctx: &GenerateCtx, markers: &[_]| {
        markers
            .iter()
            .copied()
            .map(|(kind, type_names)| {
                let types: HashSet<_> = ctx.children_of_many(type_names);
                let unprocessed_types: HashSet<_> =
                    types.difference(&processed_tys).copied().collect();
                processed_tys.extend(unprocessed_types.iter().copied());
                (kind, unprocessed_types)
            })
            .collect()
    };

    #[rustfmt::skip]
    let generate_code_for = vec![
        GenerateCodeFor {
            template: template_dir.join("Meta.ml"),
            target: output_dir.join("Generated_Meta.ml"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl(None), &[
                    "File",
                    "Span",
                    "AttrInfo",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("Values.ml"),
            target: output_dir.join("Generated_Values.ml"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl(Some(DeriveVisitors {
                    ancestors: &["big_int"],
                    name: "literal",
                    reduce: true,
                    extra_types: &[],
                })), &[
                    "Literal",
                    "IntegerTy",
                    "LiteralTy",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("Types.ml"),
            target: output_dir.join("Generated_Types.ml"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl(Some(DeriveVisitors {
                    ancestors: &["literal"],
                    name: "const_generic",
                    reduce: true,
                    extra_types: &[],
                })), &[
                    "TypeVarId",
                    "ConstGeneric",
                    "TraitClauseId",
                    "DeBruijnVar",
                    "AnyTransId",
                ]),
                // Can't merge into above because aeneas uses the above alongside their own partial
                // copy of `ty`, which causes method type clashes.
                (GenerationKind::TypeDecl(Some(DeriveVisitors {
                    ancestors: &["ty_base_base"],
                    name: "ty",
                    reduce: false,
                    extra_types: &["span"],
                })), &[
                    "TyKind",
                    "TraitImplRef",
                    "FunDeclRef",
                    "GlobalDeclRef",
                ]),
                // TODO: can't merge into above because of field name clashes (`types`, `regions` etc).
                (GenerationKind::TypeDecl(Some(DeriveVisitors {
                    ancestors: &["ty"],
                    name: "type_decl",
                    reduce: false,
                    extra_types: &[
                        "attr_info"
                    ],
                })), &[
                    "Binder",
                    "AbortKind",
                    "TypeDecl",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("Expressions.ml"),
            target: output_dir.join("Generated_Expressions.ml"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl(Some(DeriveVisitors {
                    ancestors: &["type_decl"],
                    name: "rvalue",
                    reduce: false,
                    extra_types: &[],
                })), &[
                    "Rvalue",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("GAst.ml"),
            target: output_dir.join("Generated_GAst.ml"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl(Some(DeriveVisitors {
                    ancestors: &["rvalue"],
                    name: "fun_sig",
                    reduce: false,
                    extra_types: &[],
                })), &[
                    "Call",
                    "Assert",
                    "ItemKind",
                    "Locals",
                    "FunSig",
                    "CopyNonOverlapping",
                ]),
                // These have to be kept separate to avoid field name clashes
                (GenerationKind::TypeDecl(Some(DeriveVisitors {
                    ancestors: &["fun_sig"],
                    name: "global_decl",
                    reduce: false,
                    extra_types: &[],
                })), &[
                    "GlobalDecl",
                ]),
                (GenerationKind::TypeDecl(Some(DeriveVisitors {
                    ancestors: &["global_decl"],
                    name: "trait_decl",
                    reduce: false,
                    extra_types: &[],
                })), &[
                    "TraitDecl",
                ]),
                (GenerationKind::TypeDecl(Some(DeriveVisitors {
                    ancestors: &["trait_decl"],
                    name: "trait_impl",
                    reduce: false,
                    extra_types: &[],
                })), &[
                    "TraitImpl",
                ]),
                (GenerationKind::TypeDecl(None), &[
                    "CliOpts",
                    "GExprBody",
                    "DeclarationGroup",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("LlbcAst.ml"),
            target: output_dir.join("Generated_LlbcAst.ml"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl(Some(DeriveVisitors {
                    name: "statement_base",
                    ancestors: &["trait_impl"],
                    reduce: false,
                    extra_types: &[],
                })), &[
                    "charon_lib::ast::llbc_ast::Statement",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("UllbcAst.ml"),
            target: output_dir.join("Generated_UllbcAst.ml"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl(Some(DeriveVisitors {
                    ancestors: &["trait_impl"],
                    name: "statement",
                    reduce: false,
                    extra_types: &[],
                })), &[
                    "charon_lib::ast::ullbc_ast::Statement",
                    "charon_lib::ast::ullbc_ast::SwitchTargets",
                ]),
                // TODO: Can't merge with above because of field name clashes (`content` and `span`).
                (GenerationKind::TypeDecl(Some(DeriveVisitors {
                    ancestors: &["statement"],
                    name: "ullbc_ast",
                    reduce: false,
                    extra_types: &[],
                })), &[
                    "charon_lib::ast::ullbc_ast::BodyContents",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("GAstOfJson.ml"),
            target: output_dir.join("Generated_GAstOfJson.ml"),
            markers: vec![(GenerationKind::OfJson, gast_types)],
        },
        GenerateCodeFor {
            template: template_dir.join("LlbcOfJson.ml"),
            target: output_dir.join("Generated_LlbcOfJson.ml"),
            markers: vec![(GenerationKind::OfJson, llbc_types)],
        },
        GenerateCodeFor {
            template: template_dir.join("UllbcOfJson.ml"),
            target: output_dir.join("Generated_UllbcOfJson.ml"),
            markers: vec![(GenerationKind::OfJson, ullbc_types)],
        },
    ];
    for file in generate_code_for {
        file.generate(&ctx)?;
    }
    Ok(())
}
