//! Implementations for [crate::gast]

use crate::ast::*;

impl FnPtrKind {
    pub fn mk_builtin(aid: BuiltinFunId) -> Self {
        Self::Fun(FunId::Builtin(aid))
    }
}

impl Body {
    /// Whether there is an actual body with statements etc, as opposed to the body being missing
    /// for some reason.
    pub fn has_contents(&self) -> bool {
        match self {
            Body::Unstructured(..) | Body::Structured(..) => true,
            Body::TraitMethodWithoutDefault | Body::Opaque | Body::Missing | Body::Error(..) => {
                false
            }
        }
    }

    pub fn locals(&self) -> &Locals {
        match self {
            Body::Structured(body) => &body.locals,
            Body::Unstructured(body) => &body.locals,
            _ => panic!("called `locals` on a missing body"),
        }
    }
}

impl Locals {
    pub fn new(arg_count: usize) -> Self {
        Self {
            arg_count,
            locals: Default::default(),
        }
    }

    /// Creates a new variable and returns a place pointing to it.
    /// Warning: don't forget to `StorageLive` it before using it.
    pub fn new_var(&mut self, name: Option<String>, ty: Ty) -> Place {
        let local_id = self.locals.push_with(|index| Local {
            index,
            name,
            span: Span::dummy(),
            ty: ty.clone(),
        });
        Place::new(local_id, ty)
    }

    /// Gets a place pointing to the corresponding variable.
    pub fn place_for_var(&self, local_id: LocalId) -> Place {
        let ty = self.locals[local_id].ty.clone();
        Place::new(local_id, ty)
    }

    /// Returns whether this local is the special return local or one of the input argument locals.
    pub fn is_return_or_arg(&self, lid: LocalId) -> bool {
        lid.index() <= self.arg_count
    }

    /// The place where we write the return value.
    pub fn return_place(&self) -> Place {
        self.place_for_var(LocalId::new(0))
    }

    /// Locals that aren't arguments or return values.
    pub fn non_argument_locals(&self) -> impl Iterator<Item = (LocalId, &Local)> {
        self.locals.iter_indexed().skip(1 + self.arg_count)
    }
}

impl std::ops::Index<LocalId> for Locals {
    type Output = Local;
    fn index(&self, local_id: LocalId) -> &Self::Output {
        &self.locals[local_id]
    }
}
impl std::ops::IndexMut<LocalId> for Locals {
    fn index_mut(&mut self, local_id: LocalId) -> &mut Self::Output {
        &mut self.locals[local_id]
    }
}

impl FunDecl {
    /// Replace the generic parameters of this function with the ones given by the binder.
    pub fn substitute_params(self, subst: Binder<GenericArgs>) -> Self {
        let FunDecl {
            def_id,
            item_meta,
            generics: _,
            signature,
            src,
            is_global_initializer,
            body,
        } = self;
        let signature = signature.substitute(&subst.skip_binder);
        let src = src.substitute(&subst.skip_binder);
        let body = body.substitute(&subst.skip_binder);
        FunDecl {
            def_id,
            item_meta,
            generics: subst.params,
            signature,
            src,
            is_global_initializer,
            body,
        }
    }
}
impl TraitDecl {
    pub fn methods(&self) -> impl Iterator<Item = &Binder<TraitMethod>> {
        self.methods.iter()
    }
}
impl TraitImpl {
    pub fn methods(&self) -> impl Iterator<Item = &(TraitItemName, Binder<FunDeclRef>)> {
        self.methods.iter()
    }
}

impl Binder<TraitAssocTy> {
    pub fn name(&self) -> &TraitItemName {
        &self.skip_binder.name
    }
}
impl Binder<TraitMethod> {
    pub fn name(&self) -> TraitItemName {
        self.skip_binder.name
    }
}
