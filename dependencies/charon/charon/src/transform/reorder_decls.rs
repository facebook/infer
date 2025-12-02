//! Compute an ordering on declarations that:
//! - Detects mutually-recursive groups;
//! - Always orders an item before any of its uses (except for recursive cases);
//! - Otherwise keeps a stable order.
//!
//! Aeneas needs this because proof assistant languages are sensitive to declaration order and need
//! to be explicit about mutual recursion. This should come useful for translation to any other
//! language with these properties.
use crate::common::*;
use crate::formatter::{FmtCtx, IntoFormatter};
use crate::graphs::*;
use crate::pretty::FmtWithCtx;
use crate::transform::TransformCtx;
use crate::ullbc_ast::*;
use derive_generic_visitor::*;
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use macros::{EnumAsGetters, EnumIsA, VariantIndexArity, VariantName};
use petgraph::algo::tarjan_scc;
use petgraph::graphmap::DiGraphMap;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Display, Error};
use std::vec::Vec;

use super::ctx::TransformPass;

/// A (group of) top-level declaration(s), properly reordered.
/// "G" stands for "generic"
#[derive(
    Debug, Clone, VariantIndexArity, VariantName, EnumAsGetters, EnumIsA, Serialize, Deserialize,
)]
#[charon::variants_suffix("Group")]
pub enum GDeclarationGroup<Id> {
    /// A non-recursive declaration
    NonRec(Id),
    /// A (group of mutually) recursive declaration(s)
    Rec(Vec<Id>),
}

/// A (group of) top-level declaration(s), properly reordered.
#[derive(
    Debug, Clone, VariantIndexArity, VariantName, EnumAsGetters, EnumIsA, Serialize, Deserialize,
)]
#[charon::variants_suffix("Group")]
pub enum DeclarationGroup {
    /// A type declaration group
    Type(GDeclarationGroup<TypeDeclId>),
    /// A function declaration group
    Fun(GDeclarationGroup<FunDeclId>),
    /// A global declaration group
    Global(GDeclarationGroup<GlobalDeclId>),
    ///
    TraitDecl(GDeclarationGroup<TraitDeclId>),
    ///
    TraitImpl(GDeclarationGroup<TraitImplId>),
    /// Anything that doesn't fit into these categories.
    Mixed(GDeclarationGroup<AnyTransId>),
}

impl<Id: Copy> GDeclarationGroup<Id> {
    pub fn get_ids(&self) -> &[Id] {
        use GDeclarationGroup::*;
        match self {
            NonRec(id) => std::slice::from_ref(id),
            Rec(ids) => ids.as_slice(),
        }
    }

    pub fn get_any_trans_ids(&self) -> Vec<AnyTransId>
    where
        Id: Into<AnyTransId>,
    {
        self.get_ids().iter().copied().map(|id| id.into()).collect()
    }

    fn make_group(is_rec: bool, gr: impl Iterator<Item = AnyTransId>) -> Self
    where
        Id: TryFrom<AnyTransId>,
        Id::Error: Debug,
    {
        let gr: Vec<_> = gr.map(|x| x.try_into().unwrap()).collect();
        if is_rec {
            GDeclarationGroup::Rec(gr)
        } else {
            assert!(gr.len() == 1);
            GDeclarationGroup::NonRec(gr[0])
        }
    }

    fn to_mixed(&self) -> GDeclarationGroup<AnyTransId>
    where
        Id: Into<AnyTransId>,
    {
        match self {
            GDeclarationGroup::NonRec(x) => GDeclarationGroup::NonRec((*x).into()),
            GDeclarationGroup::Rec(_) => GDeclarationGroup::Rec(self.get_any_trans_ids()),
        }
    }
}

impl DeclarationGroup {
    pub fn to_mixed_group(&self) -> GDeclarationGroup<AnyTransId> {
        use DeclarationGroup::*;
        match self {
            Type(gr) => gr.to_mixed(),
            Fun(gr) => gr.to_mixed(),
            Global(gr) => gr.to_mixed(),
            TraitDecl(gr) => gr.to_mixed(),
            TraitImpl(gr) => gr.to_mixed(),
            Mixed(gr) => gr.clone(),
        }
    }

    pub fn get_ids(&self) -> Vec<AnyTransId> {
        use DeclarationGroup::*;
        match self {
            Type(gr) => gr.get_any_trans_ids(),
            Fun(gr) => gr.get_any_trans_ids(),
            Global(gr) => gr.get_any_trans_ids(),
            TraitDecl(gr) => gr.get_any_trans_ids(),
            TraitImpl(gr) => gr.get_any_trans_ids(),
            Mixed(gr) => gr.get_any_trans_ids(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct DeclInfo {
    pub is_transparent: bool,
}

pub type DeclarationsGroups = Vec<DeclarationGroup>;

impl<Id: Display> Display for GDeclarationGroup<Id> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), Error> {
        match self {
            GDeclarationGroup::NonRec(id) => write!(f, "non-rec: {id}"),
            GDeclarationGroup::Rec(ids) => {
                write!(
                    f,
                    "rec: {}",
                    pretty_display_list(|id| format!("    {id}"), ids)
                )
            }
        }
    }
}

impl Display for DeclarationGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), Error> {
        match self {
            DeclarationGroup::Type(decl) => write!(f, "{{ Type(s): {decl} }}"),
            DeclarationGroup::Fun(decl) => write!(f, "{{ Fun(s): {decl} }}"),
            DeclarationGroup::Global(decl) => write!(f, "{{ Global(s): {decl} }}"),
            DeclarationGroup::TraitDecl(decl) => write!(f, "{{ Trait decls(s): {decl} }}"),
            DeclarationGroup::TraitImpl(decl) => write!(f, "{{ Trait impl(s): {decl} }}"),
            DeclarationGroup::Mixed(decl) => write!(f, "{{ Mixed items: {decl} }}"),
        }
    }
}

#[derive(Visitor)]
pub struct Deps {
    dgraph: DiGraphMap<AnyTransId, ()>,
    // Want to make sure we remember the order of insertion
    graph: IndexMap<AnyTransId, IndexSet<AnyTransId>>,
    // We use this when computing the graph
    current_id: Option<AnyTransId>,
    // We use this to track the trait impl block the current item belongs to
    // (if relevant).
    //
    // We use this to ignore the references to the parent impl block.
    //
    // If we don't do so, when computing our dependency graph we end up with
    // mutually recursive trait impl blocks/trait method impls in the presence
    // of associated types (the deepest reason is that we don't normalize the
    // types we query from rustc when translating the types from function
    // signatures - we avoid doing so because as of now it makes resolving
    // the trait params harder: if we get normalized types, we have to
    // implement a normalizer on our side to make sure we correctly match
    // types...).
    //
    //
    // For instance, the problem happens if in Rust we have:
    // ```text
    // pub trait WithConstTy {
    //     type W;
    //     fn f(x: &mut Self::W);
    // }
    //
    // impl WithConstTy for bool {
    //     type W = u64;
    //     fn f(_: &mut Self::W) {}
    // }
    // ```
    //
    // In LLBC we get:
    //
    // ```text
    // impl traits::Bool::0 : traits::WithConstTy<bool>
    // {
    //     type W = u64 with []
    //     fn f = traits::Bool::0::f
    // }
    //
    // fn traits::Bool::0::f<@R0>(@1: &@R0 mut (traits::Bool::0::W)) { .. }
    // //                                       ^^^^^^^^^^^^^^^
    // //                                    refers to the trait impl
    // ```
    parent_trait_impl: Option<TraitImplId>,
    parent_trait_decl: Option<TraitDeclId>,
}

impl Deps {
    fn new() -> Self {
        Deps {
            dgraph: DiGraphMap::new(),
            graph: IndexMap::new(),
            current_id: None,
            parent_trait_impl: None,
            parent_trait_decl: None,
        }
    }

    fn set_impl_or_trait_id(&mut self, kind: &ItemKind) {
        match kind {
            ItemKind::TraitDecl { trait_ref, .. } => self.parent_trait_decl = Some(trait_ref.id),
            ItemKind::TraitImpl { impl_ref, .. } => self.parent_trait_impl = Some(impl_ref.id),
            _ => {}
        }
    }
    fn set_current_id(&mut self, ctx: &TransformCtx, id: AnyTransId) {
        self.insert_node(id);
        self.current_id = Some(id);

        // Add the id of the impl/trait this item belongs to, if necessary
        use AnyTransId::*;
        match id {
            TraitDecl(_) | TraitImpl(_) | Type(_) => (),
            Global(id) => {
                if let Some(decl) = ctx.translated.global_decls.get(id) {
                    self.set_impl_or_trait_id(&decl.kind);
                }
            }
            Fun(id) => {
                if let Some(decl) = ctx.translated.fun_decls.get(id) {
                    self.set_impl_or_trait_id(&decl.kind);
                }
            }
        }
    }

    fn unset_current_id(&mut self) {
        self.current_id = None;
        self.parent_trait_impl = None;
        self.parent_trait_decl = None;
    }

    fn insert_node(&mut self, id: AnyTransId) {
        // We have to be careful about duplicate nodes
        if !self.dgraph.contains_node(id) {
            self.dgraph.add_node(id);
            assert!(!self.graph.contains_key(&id));
            self.graph.insert(id, IndexSet::new());
        }
    }

    fn insert_edge(&mut self, id1: AnyTransId) {
        let id0 = self.current_id.unwrap();
        self.insert_node(id1);
        if !self.dgraph.contains_edge(id0, id1) {
            self.dgraph.add_edge(id0, id1, ());
            self.graph.get_mut(&id0).unwrap().insert(id1);
        }
    }
}

impl VisitAst for Deps {
    fn enter_type_decl_id(&mut self, id: &TypeDeclId) {
        self.insert_edge((*id).into());
    }

    fn enter_global_decl_id(&mut self, id: &GlobalDeclId) {
        self.insert_edge((*id).into());
    }

    fn enter_trait_impl_id(&mut self, id: &TraitImplId) {
        // If the impl is the impl this item belongs to, we ignore it
        // TODO: this is not very satisfying but this is the only way we have of preventing
        // mutually recursive groups between method impls and trait impls in the presence of
        // associated types...
        if let Some(impl_id) = &self.parent_trait_impl
            && impl_id == id
        {
            return;
        }
        self.insert_edge((*id).into());
    }

    fn enter_trait_decl_id(&mut self, id: &TraitDeclId) {
        // If the trait is the trait this item belongs to, we ignore it. This is to avoid mutually
        // recursive groups between e.g. traits decls and their globals. We treat methods
        // specifically.
        if let Some(trait_id) = &self.parent_trait_decl
            && trait_id == id
        {
            return;
        }
        self.insert_edge((*id).into());
    }

    fn enter_fun_decl_id(&mut self, id: &FunDeclId) {
        self.insert_edge((*id).into());
    }

    fn visit_item_meta(&mut self, _: &ItemMeta) -> ControlFlow<Self::Break> {
        // Don't look inside because trait impls contain their own id in their name.
        Continue(())
    }
    fn visit_item_kind(&mut self, _: &ItemKind) -> ControlFlow<Self::Break> {
        // Don't look inside to avoid recording a dependency from a method impl to the impl block
        // it belongs to.
        Continue(())
    }
}

impl Deps {
    fn fmt_with_ctx(&self, ctx: &FmtCtx<'_>) -> String {
        self.dgraph
            .nodes()
            .map(|node| {
                let edges = self
                    .dgraph
                    .edges(node)
                    .map(|e| format!("\n  {}", e.1.with_ctx(ctx)))
                    .collect::<Vec<String>>()
                    .join(",");

                format!("{} -> [{}\n]", node.with_ctx(ctx), edges)
            })
            .format(",\n")
            .to_string()
    }
}

fn compute_declarations_graph<'tcx>(ctx: &'tcx TransformCtx) -> Deps {
    // The order we explore the items in will dictate the final order. We do the following: std
    // items, then items from foreign crates (sorted by crate name), then local items. Within a
    // crate, we sort by file then by source order.
    let mut sorted_items = ctx.translated.all_items().collect_vec();
    // Pre-sort files to avoid costly string comparisons. Maps file ids to an index that reflects
    // ordering on the crates (with `core` and `std` sorted first) and file names.
    let sorted_file_ids: Vector<FileId, usize> = ctx
        .translated
        .files
        .all_indices()
        .sorted_by_key(|&file_id| {
            let file = &ctx.translated.files[file_id];
            let is_std = file.crate_name == "std" || file.crate_name == "core";
            (!is_std, &file.crate_name, &file.name)
        })
        .enumerate()
        .sorted_by_key(|(_i, file_id)| *file_id)
        .map(|(i, _file_id)| i)
        .collect();
    assert_eq!(
        ctx.translated.files.slot_count(),
        sorted_file_ids.slot_count()
    );
    sorted_items.sort_by_key(|item| {
        let item_meta = item.item_meta();
        let span = item_meta.span.span;
        let file_name_order = sorted_file_ids[span.file_id];
        (item_meta.is_local, file_name_order, span.beg)
    });

    let mut graph = Deps::new();
    for item in sorted_items {
        let id = item.id();
        graph.set_current_id(ctx, id);
        match item {
            AnyTransItem::Type(..) | AnyTransItem::TraitImpl(..) | AnyTransItem::Global(..) => {
                let _ = item.drive(&mut graph);
            }
            AnyTransItem::Fun(d) => {
                // Skip `d.is_global_initializer` to avoid incorrect mutual dependencies.
                // TODO: add `is_global_initializer` to `ItemKind`.
                let _ = d.signature.drive(&mut graph);
                let _ = d.body.drive(&mut graph);
                // FIXME(#514): A method declaration depends on its declaring trait because of its
                // `Self` clause. While the clause is implicit, we make sure to record the
                // dependency manually.
                if let ItemKind::TraitDecl { trait_ref, .. } = &d.kind {
                    graph.insert_edge(trait_ref.id.into());
                }
            }
            AnyTransItem::TraitDecl(d) => {
                let TraitDecl {
                    def_id: _,
                    item_meta: _,
                    generics,
                    parent_clauses,
                    consts,
                    const_defaults,
                    types,
                    type_defaults,
                    type_clauses,
                    methods,
                    vtable,
                } = d;
                // Visit the traits referenced in the generics
                let _ = generics.drive(&mut graph);

                // Visit the parent clauses
                let _ = parent_clauses.drive(&mut graph);
                assert!(type_clauses.is_empty());

                // Visit the items
                let _ = consts.drive(&mut graph);
                let _ = types.drive(&mut graph);
                let _ = type_defaults.drive(&mut graph);
                let _ = vtable.drive(&mut graph);

                // We consider that a trait decl only contains the function/constant signatures.
                // Therefore we don't explore the default const/method ids.
                for (_name, gref) in const_defaults {
                    let _ = gref.generics.drive(&mut graph);
                }
                for (_, bound_fn) in methods {
                    let id = bound_fn.skip_binder.id;
                    let _ = bound_fn.params.drive(&mut graph);
                    if let Some(decl) = ctx.translated.fun_decls.get(id) {
                        let _ = decl.signature.drive(&mut graph);
                    }
                }
            }
        }
        graph.unset_current_id();
    }
    graph
}

fn group_declarations_from_scc(
    _ctx: &TransformCtx,
    graph: Deps,
    reordered_sccs: SCCs<AnyTransId>,
) -> DeclarationsGroups {
    let reordered_sccs = &reordered_sccs.sccs;
    let mut reordered_decls: DeclarationsGroups = Vec::new();

    // Iterate over the SCC ids in the proper order
    for scc in reordered_sccs.iter() {
        if scc.is_empty() {
            // This can happen if we failed to translate the item in this group.
            continue;
        }

        // Note that the length of an SCC should be at least 1.
        let mut it = scc.iter();
        let id0 = *it.next().unwrap();
        let decl = graph.graph.get(&id0).unwrap();

        // If an SCC has length one, the declaration may be simply recursive:
        // we determine whether it is the case by checking if the def id is in
        // its own set of dependencies.
        let is_mutually_recursive = scc.len() > 1;
        let is_simply_recursive = !is_mutually_recursive && decl.contains(&id0);
        let is_rec = is_mutually_recursive || is_simply_recursive;

        let all_same_kind = scc
            .iter()
            .all(|id| id0.variant_index_arity() == id.variant_index_arity());
        let ids = scc.iter().copied();
        let group: DeclarationGroup = match id0 {
            _ if !all_same_kind => {
                DeclarationGroup::Mixed(GDeclarationGroup::make_group(is_rec, ids))
            }
            AnyTransId::Type(_) => {
                DeclarationGroup::Type(GDeclarationGroup::make_group(is_rec, ids))
            }
            AnyTransId::Fun(_) => DeclarationGroup::Fun(GDeclarationGroup::make_group(is_rec, ids)),
            AnyTransId::Global(_) => {
                DeclarationGroup::Global(GDeclarationGroup::make_group(is_rec, ids))
            }
            AnyTransId::TraitDecl(_) => {
                let gr: Vec<_> = ids.map(|x| x.try_into().unwrap()).collect();
                // Trait declarations often refer to `Self`, like below,
                // which means they are often considered as recursive by our
                // analysis. TODO: do something more precise. What is important
                // is that we never use the "whole" self clause as argument,
                // but rather projections over the self clause (like `<Self as Foo>::u`,
                // in the declaration for `Foo`).
                if gr.len() == 1 {
                    DeclarationGroup::TraitDecl(GDeclarationGroup::NonRec(gr[0]))
                } else {
                    DeclarationGroup::TraitDecl(GDeclarationGroup::Rec(gr))
                }
            }
            AnyTransId::TraitImpl(_) => {
                DeclarationGroup::TraitImpl(GDeclarationGroup::make_group(is_rec, ids))
            }
        };

        reordered_decls.push(group);
    }
    reordered_decls
}

fn compute_reordered_decls(ctx: &TransformCtx) -> DeclarationsGroups {
    trace!();

    // Step 1: explore the declarations to build the graph
    let graph = compute_declarations_graph(ctx);
    trace!("Graph:\n{}\n", graph.fmt_with_ctx(&ctx.into_fmt()));

    // Step 2: Apply Tarjan's SCC (Strongly Connected Components) algorithm
    let sccs = tarjan_scc(&graph.dgraph);

    // Step 3: Reorder the declarations in an order as close as possible to the one
    // given by the user. To be more precise, if we don't need to move
    // definitions, the order in which we generate the declarations should
    // be the same as the one in which the user wrote them.
    // Remark: the [get_id_dependencies] function will be called once per id, meaning
    // it is ok if it is not very efficient and clones values.
    let get_id_dependencies = &|id| graph.graph.get(&id).unwrap().iter().copied().collect();
    let all_ids: Vec<AnyTransId> = graph
        .graph
        .keys()
        .copied()
        // Don't list ids that weren't translated.
        .filter(|id| ctx.translated.get_item(*id).is_some())
        .collect();
    let reordered_sccs = reorder_sccs::<AnyTransId>(get_id_dependencies, &all_ids, &sccs);

    // Finally, generate the list of declarations
    let reordered_decls = group_declarations_from_scc(ctx, graph, reordered_sccs);

    trace!("{:?}", reordered_decls);
    reordered_decls
}

pub struct Transform;
impl TransformPass for Transform {
    fn transform_ctx(&self, ctx: &mut TransformCtx) {
        let reordered_decls = compute_reordered_decls(&ctx);
        ctx.translated.ordered_decls = Some(reordered_decls);
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_reorder_sccs1() {
        use std::collections::BTreeSet as OrdSet;
        let sccs = vec![vec![0], vec![1, 2], vec![3, 4, 5]];
        let ids = vec![0, 1, 2, 3, 4, 5];

        let get_deps = &|x| match x {
            0 => vec![3],
            1 => vec![0, 3],
            _ => vec![],
        };
        let reordered = crate::reorder_decls::reorder_sccs(get_deps, &ids, &sccs);

        assert!(reordered.sccs == vec![vec![3, 4, 5], vec![0], vec![1, 2],]);
        assert!(reordered.scc_deps[0] == OrdSet::from([]));
        assert!(reordered.scc_deps[1] == OrdSet::from([0]));
        assert!(reordered.scc_deps[2] == OrdSet::from([0, 1]));
    }
}
