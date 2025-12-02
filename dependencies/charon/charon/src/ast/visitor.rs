//! Defines two overrideable visitor traits that can be used to conveniently traverse the whole
//! contents of an item. This is useful when e.g. dealing with types, which show up pretty much
//! everywhere in the ast.
//!
//! The crate defines two traits:
/// - `AstVisitable` is a trait implemented by all the types that can be visited by this;
/// - `VisitAst[Mut]` is a (pair of) visitor trait(s) that can be implemented by visitors.
/// To define a visitor, implement `VisitAst[Mut]` and override the methods you need. Calling
/// `x.drive[_mut](&mut visitor)` will then traverse `x`, calling the visitor methods on all the
/// subvalues encountered.
///
/// Underneath it all, this uses `derive_generic_visitor::Drive[Mut]` to do the actual visiting.
use std::{any::Any, collections::HashMap};

use crate::ast::*;
use derive_generic_visitor::*;
use index_vec::Idx;
use indexmap::IndexMap;

/// An overrideable visitor trait that can be used to conveniently traverse the whole contents of
/// an item. This is useful when e.g. dealing with types, which show up pretty much everywhere in
/// the ast.
///
/// This defines three traits:
/// - `AstVisitable` is a trait implemented by all the types listed below; it has a
/// `drive[_mut]` method that takes a `VisitAst[Mut]` visitor and calls its methods on all
/// the relevant subvalues of `self` encountered.
/// - `VisitAst[Mut]` is a (pair of) visitor trait(s) that can be implemented by visitors. To
/// define a visitor, implement `VisitAst[Mut]` and override the methods you need.
///
/// This trait has a `drive[_mut]` method that knows how to drive a `VisitAst[Mut]` visitor. This
/// trait is implemented for all the listed types. If listed as `override`, the corresponding
/// visitor trait has an overrideable method to visit this type. If listed as `drive`, the type
/// will only be visited by recursing into its contents.
///
/// Morally this represents the predicate `for<V: VisitAst[Mut]> Self:
/// Drive[Mut]<AstVisitableWrapper<V>>`
#[visitable_group(
    // Defines the `Visit[Mut]` traits and the `drive[_mut]` method that drives them.
    visitor(drive(&VisitAst)),
    visitor(drive_mut(&mut VisitAstMut)),
    // Types that we unconditionally explore.
    drive(
        AbortKind, Assert, BinOp, Body, BorrowKind, BuiltinFunId, BuiltinIndexOp, BuiltinTy, Call,
        CastKind, ClosureInfo, ClosureKind, ConstGenericVar, ConstGenericVarId,
        Disambiguator, DynPredicate, Field, FieldId, FieldProjKind, FloatTy, FloatValue,
        FnOperand, FunId, FunIdOrTraitMethodRef, FunSig, ImplElem, IntegerTy, IntTy, UIntTy, Literal, LiteralTy,
        llbc_ast::ExprBody, llbc_ast::RawStatement, llbc_ast::Switch,
        Locals, Name, NullOp, Opaque, Operand, PathElem, PlaceKind, ProjectionElem, RawConstantExpr,
        RefKind, RegionId, RegionVar, ScalarValue, TraitItemName,
        TranslatedCrate, TypeDeclKind, TypeId, TypeVar, TypeVarId, llbc_ast::StatementId,
        ullbc_ast::BlockData, ullbc_ast::BlockId, ullbc_ast::ExprBody, ullbc_ast::RawStatement,
        ullbc_ast::RawTerminator, ullbc_ast::SwitchTargets,
        UnOp, UnsizingMetadata, Local, Variant, VariantId, LocalId, CopyNonOverlapping, Layout, VariantLayout, PtrMetadata, VTable,
        for<T: AstVisitable> Box<T>,
        for<T: AstVisitable> Option<T>,
        for<A: AstVisitable, B: AstVisitable> (A, B),
        for<A: AstVisitable, B: AstVisitable, C: AstVisitable> (A, B, C),
        for<A: AstVisitable, B: AstVisitable> Result<A, B>,
        for<A: AstVisitable, B: AstVisitable> OutlivesPred<A, B>,
        for<T: AstVisitable> Vec<T>,
        for<I: Idx, T: AstVisitable> Vector<I, T>,
    ),
    // Types for which we call the corresponding `visit_$ty` method, which by default explores the
    // type but can be overridden.
    override(
        DeBruijnId, Ty, TyKind, Region, ConstGeneric, TraitRef, TraitRefKind,
        TypeDeclRef, FunDeclRef, MaybeBuiltinFunDeclRef, TraitMethodRef, GlobalDeclRef, TraitDeclRef, TraitImplRef,
        GenericArgs, GenericParams, TraitClause, TraitClauseId, TraitTypeConstraint, Place, Rvalue,
        for<T: AstVisitable + Idx> DeBruijnVar<T>,
        for<T: AstVisitable> RegionBinder<T>,
        for<T: AstVisitable> Binder<T>,
        llbc_block: llbc_ast::Block, llbc_statement: llbc_ast::Statement,
        ullbc_statement: ullbc_ast::Statement, ullbc_terminator: ullbc_ast::Terminator,
        AggregateKind, FnPtr, ItemKind, ItemMeta, Span, ConstantExpr,
        FunDeclId, GlobalDeclId, TypeDeclId, TraitDeclId, TraitImplId,
        FunDecl, GlobalDecl, TypeDecl, TraitDecl, TraitImpl,
    )
)]
pub trait AstVisitable: Any {
    /// The name of the type, used for debug logging.
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
    /// Visit all occurrences of that type inside `self`, in pre-order traversal.
    fn dyn_visit<T: AstVisitable>(&self, f: impl FnMut(&T)) {
        let _ = self.drive(&mut DynVisitor::new_shared::<T>(f));
    }
    /// Visit all occurrences of that type inside `self`, in pre-order traversal.
    fn dyn_visit_mut<T: AstVisitable>(&mut self, f: impl FnMut(&mut T)) {
        let _ = self.drive_mut(&mut DynVisitor::new_mut::<T>(f));
    }
}

/// Manual impl that only visits the values
impl<K: Any, T: AstVisitable> AstVisitable for HashMap<K, T> {
    fn drive<V: VisitAst>(&self, v: &mut V) -> ControlFlow<V::Break> {
        for x in self.values() {
            v.visit(x)?;
        }
        Continue(())
    }
    fn drive_mut<V: VisitAstMut>(&mut self, v: &mut V) -> ControlFlow<V::Break> {
        for x in self.values_mut() {
            v.visit(x)?;
        }
        Continue(())
    }
}

/// Manual impl that only visits the values
impl<K: Any, T: AstVisitable> AstVisitable for IndexMap<K, T> {
    fn drive<V: VisitAst>(&self, v: &mut V) -> ControlFlow<V::Break> {
        for x in self.values() {
            v.visit(x)?;
        }
        Continue(())
    }
    fn drive_mut<V: VisitAstMut>(&mut self, v: &mut V) -> ControlFlow<V::Break> {
        for x in self.values_mut() {
            v.visit(x)?;
        }
        Continue(())
    }
}

/// A smaller visitor group just for function bodies. This explores statements, places and
/// operands, but does not recurse into types.
///
/// This defines three traits:
/// - `BodyVisitable` is a trait implemented by all the types listed below; it has a
/// `drive_body[_mut]` method that takes a `VisitBody[Mut]` visitor and calls its methods on all
/// the relevant subvalues of `self` encountered.
/// - `VisitBody[Mut]` is a (pair of) visitor trait(s) that can be implemented by visitors. To
/// define a visitor, implement `VisitBody[Mut]` and override the methods you need.
///
/// Morally this represents the predicate `for<V: VisitBody[Mut]> Self:
/// Drive[Mut]<BodyVisitableWrapper<V>>`
#[visitable_group(
    // Defines the `VisitBody[Mut]` traits and the `drive_body[_mut]` method that drives them.
    visitor(drive_body(&VisitBody)),
    visitor(drive_body_mut(&mut VisitBodyMut)),
    // Types that are ignored when encountered.
    skip(
        AbortKind, BinOp, BorrowKind, ConstantExpr, ConstGeneric, FieldId, FieldProjKind,
        TypeDeclRef, FunDeclId, FunIdOrTraitMethodRef, GenericArgs, GlobalDeclRef, IntegerTy, IntTy, UIntTy,
        NullOp, RefKind, ScalarValue, Span, Ty, TypeDeclId, TypeId, UnOp, VariantId, LocalId,
        TraitRef,
    ),
    // Types that we unconditionally explore.
    drive(
        Assert, PlaceKind,
        llbc_ast::ExprBody, llbc_ast::RawStatement, llbc_ast::Switch,
        ullbc_ast::BlockData, ullbc_ast::ExprBody, ullbc_ast::RawStatement,
        ullbc_ast::RawTerminator, ullbc_ast::SwitchTargets, CopyNonOverlapping,
        llbc_ast::StatementId,
        Body, Opaque, Locals, Local,
        for<T: BodyVisitable> Box<T>,
        for<T: BodyVisitable> Option<T>,
        for<T: BodyVisitable, E: BodyVisitable> Result<T, E>,
        for<A: BodyVisitable, B: BodyVisitable> (A, B),
        for<A: BodyVisitable, B: BodyVisitable, C: BodyVisitable> (A, B, C),
        for<T: BodyVisitable> Vec<T>,
        for<I: Idx, T: BodyVisitable> Vector<I, T>,
    ),
    // Types for which we call the corresponding `visit_$ty` method, which by default explores the
    // type but can be overridden.
    override(
        AggregateKind, Call, FnOperand, FnPtr,
        Operand, Place, ProjectionElem, Rvalue,
        llbc_block: llbc_ast::Block,
        llbc_statement: llbc_ast::Statement,
        ullbc_statement: ullbc_ast::Statement,
        ullbc_terminator: ullbc_ast::Terminator,
        ullbc_block_id: ullbc_ast::BlockId,
    )
)]
pub trait BodyVisitable: Any {
    /// Visit all occurrences of that type inside `self`, in pre-order traversal.
    fn dyn_visit_in_body<T: BodyVisitable>(&self, f: impl FnMut(&T)) {
        let _ = self.drive_body(&mut DynVisitor::new_shared::<T>(f));
    }

    /// Visit all occurrences of that type inside `self`, in pre-order traversal.
    fn dyn_visit_in_body_mut<T: BodyVisitable>(&mut self, f: impl FnMut(&mut T)) {
        let _ = self.drive_body_mut(&mut DynVisitor::new_mut::<T>(f));
    }
}

/// Ast and body visitor that uses dynamic dispatch to call the provided function on the visited
/// values of the right type.
#[derive(Visitor)]
pub struct DynVisitor<F> {
    enter: F,
}
impl DynVisitor<()> {
    pub fn new_shared<T: Any>(mut f: impl FnMut(&T)) -> DynVisitor<impl FnMut(&dyn Any)> {
        let enter = move |x: &dyn Any| {
            if let Some(x) = x.downcast_ref::<T>() {
                f(x);
            }
        };
        DynVisitor { enter }
    }
    pub fn new_mut<T: Any>(mut f: impl FnMut(&mut T)) -> DynVisitor<impl FnMut(&mut dyn Any)> {
        let enter = move |x: &mut dyn Any| {
            if let Some(x) = x.downcast_mut::<T>() {
                f(x);
            }
        };
        DynVisitor { enter }
    }
}
impl<F> VisitAst for DynVisitor<F>
where
    F: FnMut(&dyn Any),
{
    fn visit<'a, T: AstVisitable>(&'a mut self, x: &T) -> ControlFlow<Self::Break> {
        (self.enter)(x);
        x.drive(self)?;
        Continue(())
    }
}
impl<F> VisitAstMut for DynVisitor<F>
where
    F: FnMut(&mut dyn Any),
{
    fn visit<'a, T: AstVisitable>(&'a mut self, x: &mut T) -> ControlFlow<Self::Break> {
        (self.enter)(x);
        x.drive_mut(self)?;
        Continue(())
    }
}
impl<F> VisitBody for DynVisitor<F>
where
    F: FnMut(&dyn Any),
{
    fn visit<'a, T: BodyVisitable>(&'a mut self, x: &T) -> ControlFlow<Self::Break> {
        (self.enter)(x);
        x.drive_body(self)?;
        Continue(())
    }
}
impl<F> VisitBodyMut for DynVisitor<F>
where
    F: FnMut(&mut dyn Any),
{
    fn visit<'a, T: BodyVisitable>(&'a mut self, x: &mut T) -> ControlFlow<Self::Break> {
        (self.enter)(x);
        x.drive_body_mut(self)?;
        Continue(())
    }
}

pub use wrappers::*;
mod wrappers {
    //! This module defines a bunch of visitor wrappers, in the model described in the `derive_generic_visitor` crate.
    //! Each such wrapper is a non-recursive visitor; the only thing it does is that its `.visit()`
    //! method calls into the appropriate `visit_foo` of the wrapper, then continues visiting with
    //! the wrapped visitor.
    //!
    //! To use such a wrapper, just override the `visit` method of your visitor to call
    //! `TheWrapper::new(self).visit(x)`. This will integrate the wrapper into the normal behavior of
    //! your visitor.
    //!
    //! Each wrapper interacts with its wrapped visitor via a trait. To be able to use several
    //! wrappers at once, they must implement the wrapper-specific trait themselves and forward to
    //! their wrappee visitor. It's a bit annoying as that potentially requires N^2 impls. I don't
    //! know of a better design.
    use std::mem;

    use crate::ast::*;
    use derive_generic_visitor::*;

    /// Struct that we use to be able to use our visitor wrappers with each other to share
    /// functionality, while still making the wrappers composable. We can implement e.g.
    /// `VisitorWithItem for DontLeakImplDetails<Wrapper<V>>` while still retaining the capacity to
    /// implement `impl<V: VisitorWithItem> VisitorWithItem for Wrapper<V>` that forwards to the
    /// inner visitor.
    #[repr(transparent)]
    pub struct DontLeakImplDetails<V>(V);

    impl<V> DontLeakImplDetails<V> {
        pub fn new(v: &mut V) -> &mut Self {
            // SAFETY: `repr(transparent)`
            unsafe { std::mem::transmute(v) }
        }
        pub fn inner(&mut self) -> &mut V {
            // SAFETY: `repr(transparent)`
            unsafe { std::mem::transmute(self) }
        }
    }

    impl<V: Visitor> Visitor for DontLeakImplDetails<V> {
        type Break = V::Break;
    }
    impl<V: VisitAst> VisitAst for DontLeakImplDetails<V> {
        /// Just forward to the wrapped visitor.
        fn visit_inner<T>(&mut self, x: &T) -> ControlFlow<Self::Break>
        where
            T: AstVisitable,
        {
            x.drive(self.inner())
        }
    }
    impl<V: VisitAstMut> VisitAstMut for DontLeakImplDetails<V> {
        /// Just forward to the wrapped visitor.
        fn visit_inner<T>(&mut self, x: &mut T) -> ControlFlow<Self::Break>
        where
            T: AstVisitable,
        {
            x.drive_mut(self.inner())
        }
    }

    /// Visitor wrapper that tracks the depth of binders. To use it, make a visitor that implements
    /// `VisitorWithBinderDepth` and override its `visit` function as follows:
    /// ```ignore
    /// impl VisitAst for MyVisitor {
    ///     fn visit<'a, T: AstVisitable>(&'a mut self, x: &T) -> ControlFlow<Self::Break> {
    ///         VisitWithBinderDepth::new(self).visit(x)
    ///     }
    ///     ...
    /// }
    /// ```
    #[repr(transparent)]
    pub struct VisitWithBinderDepth<V>(V);

    impl<V: VisitorWithBinderDepth> VisitWithBinderDepth<V> {
        pub fn new(v: &mut V) -> &mut Self {
            // SAFETY: `repr(transparent)`
            unsafe { std::mem::transmute(v) }
        }
        pub fn inner(&mut self) -> &mut V {
            // SAFETY: `repr(transparent)`
            unsafe { std::mem::transmute(self) }
        }
    }

    pub trait VisitorWithBinderDepth {
        fn binder_depth_mut(&mut self) -> &mut DeBruijnId;
    }

    impl<V: Visitor> Visitor for VisitWithBinderDepth<V> {
        type Break = V::Break;
    }
    impl<V: VisitAst + VisitorWithBinderDepth> VisitAst for VisitWithBinderDepth<V> {
        fn visit_inner<T>(&mut self, x: &T) -> ControlFlow<Self::Break>
        where
            T: AstVisitable,
        {
            x.drive(self.inner())
        }
        fn enter_region_binder<T: AstVisitable>(&mut self, _: &RegionBinder<T>) {
            let binder_depth = self.0.binder_depth_mut();
            *binder_depth = binder_depth.incr()
        }
        fn exit_region_binder<T: AstVisitable>(&mut self, _: &RegionBinder<T>) {
            let binder_depth = self.0.binder_depth_mut();
            *binder_depth = binder_depth.decr()
        }
        fn enter_binder<T: AstVisitable>(&mut self, _: &Binder<T>) {
            let binder_depth = self.0.binder_depth_mut();
            *binder_depth = binder_depth.incr()
        }
        fn exit_binder<T: AstVisitable>(&mut self, _: &Binder<T>) {
            let binder_depth = self.0.binder_depth_mut();
            *binder_depth = binder_depth.decr()
        }
    }
    impl<V: VisitAstMut + VisitorWithBinderDepth> VisitAstMut for VisitWithBinderDepth<V> {
        fn visit_inner<T>(&mut self, x: &mut T) -> ControlFlow<Self::Break>
        where
            T: AstVisitable,
        {
            x.drive_mut(self.inner())
        }
        fn enter_region_binder<T: AstVisitable>(&mut self, _: &mut RegionBinder<T>) {
            let binder_depth = self.0.binder_depth_mut();
            *binder_depth = binder_depth.incr()
        }
        fn exit_region_binder<T: AstVisitable>(&mut self, _: &mut RegionBinder<T>) {
            let binder_depth = self.0.binder_depth_mut();
            *binder_depth = binder_depth.decr()
        }
        fn enter_binder<T: AstVisitable>(&mut self, _: &mut Binder<T>) {
            let binder_depth = self.0.binder_depth_mut();
            *binder_depth = binder_depth.incr()
        }
        fn exit_binder<T: AstVisitable>(&mut self, _: &mut Binder<T>) {
            let binder_depth = self.0.binder_depth_mut();
            *binder_depth = binder_depth.decr()
        }
    }

    /// Visitor wrapper that adds item-generic `enter_item` and `exit_item` methods.
    #[repr(transparent)]
    pub struct VisitWithItem<V>(V);

    impl<V> VisitWithItem<V> {
        pub fn new(v: &mut V) -> &mut Self {
            // SAFETY: `repr(transparent)`
            unsafe { std::mem::transmute(v) }
        }
        pub fn inner(&mut self) -> &mut V {
            // SAFETY: `repr(transparent)`
            unsafe { std::mem::transmute(self) }
        }
    }

    pub trait VisitorWithItem: VisitAst {
        fn enter_item(&mut self, _item: AnyTransItem<'_>) {}
        fn exit_item(&mut self, _item: AnyTransItem<'_>) {}
        fn visit_item(&mut self, item: AnyTransItem<'_>) -> ControlFlow<Self::Break> {
            self.enter_item(item);
            item.drive(self)?;
            self.exit_item(item);
            Continue(())
        }
    }
    pub trait VisitorWithItemMut: VisitAstMut {
        fn enter_item(&mut self, _item: AnyTransItemMut<'_>) {}
        fn exit_item(&mut self, _item: AnyTransItemMut<'_>) {}
        fn visit_item(&mut self, mut item: AnyTransItemMut<'_>) -> ControlFlow<Self::Break> {
            self.enter_item(item.reborrow());
            item.drive_mut(self)?;
            self.exit_item(item);
            Continue(())
        }
    }

    impl<V: Visitor> Visitor for VisitWithItem<V> {
        type Break = V::Break;
    }
    impl<V: VisitAst + VisitorWithItem> VisitAst for VisitWithItem<V> {
        fn visit_inner<T>(&mut self, x: &T) -> ControlFlow<Self::Break>
        where
            T: AstVisitable,
        {
            x.drive(self.inner())
        }
        fn visit_fun_decl(&mut self, x: &FunDecl) -> ControlFlow<Self::Break> {
            self.0.visit_item(AnyTransItem::Fun(x))
        }
        fn visit_type_decl(&mut self, x: &TypeDecl) -> ControlFlow<Self::Break> {
            self.0.visit_item(AnyTransItem::Type(x))
        }
        fn visit_global_decl(&mut self, x: &GlobalDecl) -> ControlFlow<Self::Break> {
            self.0.visit_item(AnyTransItem::Global(x))
        }
        fn visit_trait_decl(&mut self, x: &TraitDecl) -> ControlFlow<Self::Break> {
            self.0.visit_item(AnyTransItem::TraitDecl(x))
        }
        fn visit_trait_impl(&mut self, x: &TraitImpl) -> ControlFlow<Self::Break> {
            self.0.visit_item(AnyTransItem::TraitImpl(x))
        }
    }
    impl<V: VisitAstMut + VisitorWithItemMut> VisitAstMut for VisitWithItem<V> {
        fn visit_inner<T>(&mut self, x: &mut T) -> ControlFlow<Self::Break>
        where
            T: AstVisitable,
        {
            x.drive_mut(self.inner())
        }
        fn visit_fun_decl(&mut self, x: &mut FunDecl) -> ControlFlow<Self::Break> {
            self.0.visit_item(AnyTransItemMut::Fun(x))
        }
        fn visit_type_decl(&mut self, x: &mut TypeDecl) -> ControlFlow<Self::Break> {
            self.0.visit_item(AnyTransItemMut::Type(x))
        }
        fn visit_global_decl(&mut self, x: &mut GlobalDecl) -> ControlFlow<Self::Break> {
            self.0.visit_item(AnyTransItemMut::Global(x))
        }
        fn visit_trait_decl(&mut self, x: &mut TraitDecl) -> ControlFlow<Self::Break> {
            self.0.visit_item(AnyTransItemMut::TraitDecl(x))
        }
        fn visit_trait_impl(&mut self, x: &mut TraitImpl) -> ControlFlow<Self::Break> {
            self.0.visit_item(AnyTransItemMut::TraitImpl(x))
        }
    }

    /// Visitor wrapper that tracks the stack of binders seen so far. See [`VisitWithBinderDepth`] for how to use.
    #[repr(transparent)]
    pub struct VisitWithBinderStack<V>(V);

    impl<V: VisitorWithBinderStack> VisitWithBinderStack<V> {
        // Helper
        fn wrap(v: &mut V) -> &mut Self {
            // SAFETY: `repr(transparent)`
            unsafe { std::mem::transmute(v) }
        }
        pub fn new(v: &mut V) -> &mut VisitWithItem<DontLeakImplDetails<Self>> {
            // Use the `WithItem` wrapper to simplify the implementation of this wrapper. We use
            // `DontLeakImplDetails` to use the specific `VisitorWithItem` impl we care about
            // instead of the one that forwards to the `VisitorWithItem` of the containted `V`.
            VisitWithItem::new(DontLeakImplDetails::new(Self::wrap(v)))
        }
        pub fn inner(&mut self) -> &mut V {
            // SAFETY: `repr(transparent)`
            unsafe { std::mem::transmute(self) }
        }
    }

    pub trait VisitorWithBinderStack {
        fn binder_stack_mut(&mut self) -> &mut BindingStack<GenericParams>;
    }

    impl<V: VisitAst + VisitorWithBinderStack> VisitorWithItem
        for DontLeakImplDetails<VisitWithBinderStack<V>>
    {
        fn enter_item(&mut self, item: AnyTransItem<'_>) {
            self.0
                .0
                .binder_stack_mut()
                .push(item.generic_params().clone());
        }
        fn exit_item(&mut self, _item: AnyTransItem<'_>) {
            self.0.0.binder_stack_mut().pop();
        }
    }
    impl<V: VisitAstMut + VisitorWithBinderStack> VisitorWithItemMut
        for DontLeakImplDetails<VisitWithBinderStack<V>>
    {
        fn enter_item(&mut self, item: AnyTransItemMut<'_>) {
            self.0
                .0
                .binder_stack_mut()
                .push(item.as_ref().generic_params().clone());
        }
        fn exit_item(&mut self, _item: AnyTransItemMut<'_>) {
            self.0.0.binder_stack_mut().pop();
        }
    }

    impl<V: Visitor> Visitor for VisitWithBinderStack<V> {
        type Break = V::Break;
    }
    impl<V: VisitAst + VisitorWithBinderStack> VisitAst for VisitWithBinderStack<V> {
        fn visit_inner<T>(&mut self, x: &T) -> ControlFlow<Self::Break>
        where
            T: AstVisitable,
        {
            x.drive(self.inner())
        }
        fn visit_binder<T: AstVisitable>(
            &mut self,
            binder: &Binder<T>,
        ) -> ControlFlow<Self::Break> {
            self.0.binder_stack_mut().push(binder.params.clone());
            self.visit_inner(binder)?;
            self.0.binder_stack_mut().pop();
            Continue(())
        }
        fn visit_region_binder<T: AstVisitable>(
            &mut self,
            binder: &RegionBinder<T>,
        ) -> ControlFlow<Self::Break> {
            self.0.binder_stack_mut().push(GenericParams {
                regions: binder.regions.clone(),
                ..Default::default()
            });
            self.visit_inner(binder)?;
            self.0.binder_stack_mut().pop();
            Continue(())
        }
    }
    impl<V: VisitAstMut + VisitorWithBinderStack> VisitAstMut for VisitWithBinderStack<V> {
        fn visit_inner<T>(&mut self, x: &mut T) -> ControlFlow<Self::Break>
        where
            T: AstVisitable,
        {
            x.drive_mut(self.inner())
        }
        fn visit_binder<T: AstVisitable>(
            &mut self,
            binder: &mut Binder<T>,
        ) -> ControlFlow<Self::Break> {
            self.0.binder_stack_mut().push(binder.params.clone());
            self.visit_inner(binder)?;
            self.0.binder_stack_mut().pop();
            Continue(())
        }
        fn visit_region_binder<T: AstVisitable>(
            &mut self,
            binder: &mut RegionBinder<T>,
        ) -> ControlFlow<Self::Break> {
            self.0.binder_stack_mut().push(GenericParams {
                regions: binder.regions.clone(),
                ..Default::default()
            });
            self.visit_inner(binder)?;
            self.0.binder_stack_mut().pop();
            Continue(())
        }
    }

    /// Visitor wrapper that tracks the current span. See [`VisitWithBinderDepth`] for how to use.
    #[repr(transparent)]
    pub struct VisitWithSpan<V>(V);

    impl<V: VisitorWithSpan> VisitWithSpan<V> {
        // Helper
        fn wrap(v: &mut V) -> &mut Self {
            // SAFETY: `repr(transparent)`
            unsafe { std::mem::transmute(v) }
        }
        pub fn new(v: &mut V) -> &mut VisitWithItem<DontLeakImplDetails<Self>> {
            // Use the `WithItem` wrapper to simplify the implementation of this wrapper. We use
            // `DontLeakImplDetails` to use the specific `VisitorWithItem` impl we care about
            // instead of the one that forwards to the `VisitorWithItem` of the containted `V`.
            VisitWithItem::new(DontLeakImplDetails::new(Self::wrap(v)))
        }
        pub fn inner(&mut self) -> &mut V {
            // SAFETY: `repr(transparent)`
            unsafe { std::mem::transmute(self) }
        }
    }

    pub trait VisitorWithSpan {
        fn current_span(&mut self) -> &mut Span;
    }

    impl<V: VisitAst + VisitorWithSpan> VisitorWithItem for DontLeakImplDetails<VisitWithSpan<V>> {
        fn visit_item(&mut self, item: AnyTransItem<'_>) -> ControlFlow<Self::Break> {
            let old_span = mem::replace(self.0.0.current_span(), item.item_meta().span);
            item.drive(self)?;
            *self.0.0.current_span() = old_span;
            Continue(())
        }
    }
    impl<V: VisitAstMut + VisitorWithSpan> VisitorWithItemMut
        for DontLeakImplDetails<VisitWithSpan<V>>
    {
        fn visit_item(&mut self, mut item: AnyTransItemMut<'_>) -> ControlFlow<Self::Break> {
            let span = item.as_ref().item_meta().span;
            let old_span = mem::replace(self.0.0.current_span(), span);
            item.drive_mut(self)?;
            *self.0.0.current_span() = old_span;
            Continue(())
        }
    }

    impl<V: Visitor> Visitor for VisitWithSpan<V> {
        type Break = V::Break;
    }
    impl<V: VisitAst + VisitorWithSpan> VisitWithSpan<V> {
        fn visit_inner_track_span<T>(&mut self, x: &T, span: Span) -> ControlFlow<V::Break>
        where
            T: AstVisitable,
            T: for<'s> derive_generic_visitor::Drive<'s, AstVisitableWrapper<Self>>,
        {
            let old_span = mem::replace(self.0.current_span(), span);
            self.visit_inner(x)?;
            *self.0.current_span() = old_span;
            Continue(())
        }
    }
    impl<V: VisitAstMut + VisitorWithSpan> VisitWithSpan<V> {
        fn visit_inner_mut_track_span<T>(&mut self, x: &mut T, span: Span) -> ControlFlow<V::Break>
        where
            T: AstVisitable,
            T: for<'s> derive_generic_visitor::DriveMut<'s, AstVisitableWrapper<Self>>,
        {
            let old_span = mem::replace(self.0.current_span(), span);
            self.visit_inner(x)?;
            *self.0.current_span() = old_span;
            Continue(())
        }
    }
    impl<V: VisitAst + VisitorWithSpan> VisitAst for VisitWithSpan<V> {
        fn visit_inner<T>(&mut self, x: &T) -> ControlFlow<Self::Break>
        where
            T: AstVisitable,
        {
            x.drive(self.inner())
        }
        fn visit_trait_clause(&mut self, x: &TraitClause) -> ControlFlow<Self::Break> {
            match x.span {
                Some(span) => self.visit_inner_track_span(x, span),
                None => self.visit_inner(x),
            }
        }
        fn visit_ullbc_statement(&mut self, x: &ullbc_ast::Statement) -> ControlFlow<Self::Break> {
            self.visit_inner_track_span(x, x.span)
        }
        fn visit_ullbc_terminator(
            &mut self,
            x: &ullbc_ast::Terminator,
        ) -> ControlFlow<Self::Break> {
            self.visit_inner_track_span(x, x.span)
        }
        fn visit_llbc_statement(&mut self, x: &llbc_ast::Statement) -> ControlFlow<Self::Break> {
            self.visit_inner_track_span(x, x.span)
        }
        fn visit_llbc_block(&mut self, x: &llbc_ast::Block) -> ControlFlow<Self::Break> {
            self.visit_inner_track_span(x, x.span)
        }
    }
    impl<V: VisitAstMut + VisitorWithSpan> VisitAstMut for VisitWithSpan<V> {
        fn visit_inner<T>(&mut self, x: &mut T) -> ControlFlow<Self::Break>
        where
            T: AstVisitable,
        {
            x.drive_mut(self.inner())
        }
        fn visit_trait_clause(&mut self, x: &mut TraitClause) -> ControlFlow<Self::Break> {
            match x.span {
                Some(span) => self.visit_inner_mut_track_span(x, span),
                None => self.visit_inner(x),
            }
        }
        fn visit_ullbc_statement(
            &mut self,
            x: &mut ullbc_ast::Statement,
        ) -> ControlFlow<Self::Break> {
            self.visit_inner_mut_track_span(x, x.span)
        }
        fn visit_ullbc_terminator(
            &mut self,
            x: &mut ullbc_ast::Terminator,
        ) -> ControlFlow<Self::Break> {
            self.visit_inner_mut_track_span(x, x.span)
        }
        fn visit_llbc_statement(
            &mut self,
            x: &mut llbc_ast::Statement,
        ) -> ControlFlow<Self::Break> {
            self.visit_inner_mut_track_span(x, x.span)
        }
        fn visit_llbc_block(&mut self, x: &mut llbc_ast::Block) -> ControlFlow<Self::Break> {
            self.visit_inner_mut_track_span(x, x.span)
        }
    }

    /// Combo impl to be able to use `VisitWithSpan` and `VisitWithBinderStack` together.
    impl<V: VisitorWithSpan> VisitorWithSpan for VisitWithBinderStack<V> {
        fn current_span(&mut self) -> &mut Span {
            self.0.current_span()
        }
    }
    impl<V: VisitorWithSpan> VisitorWithSpan for VisitWithItem<V> {
        fn current_span(&mut self) -> &mut Span {
            self.0.current_span()
        }
    }
    impl<V: VisitorWithSpan> VisitorWithSpan for DontLeakImplDetails<V> {
        fn current_span(&mut self) -> &mut Span {
            self.0.current_span()
        }
    }
}
