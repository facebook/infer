mod def_id;
mod mir;
mod new;
mod ty;

pub use def_id::*;
pub use mir::*;
pub use new::*;
pub use ty::*;

use crate::prelude::*;
use crate::sinto_todo;

sinto_reexport!(rustc_span::Span);

pub use rustc_span::source_map::Spanned;
impl<'s, S: UnderOwnerState<'s>, T: SInto<S, U>, U> SInto<S, Spanned<U>> for Spanned<T> {
    fn sinto<'a>(&self, s: &S) -> Spanned<U> {
        Spanned {
            node: self.node.sinto(s),
            span: self.span,
        }
    }
}

sinto_todo!(rustc_span, ErrorGuaranteed);

/// Reflects [`rustc_span::symbol::Ident`]
pub type Ident = (Symbol, Span);
impl<'tcx, S: BaseState<'tcx>> SInto<S, Ident> for rustc_span::symbol::Ident {
    fn sinto(&self, s: &S) -> Ident {
        (self.name.sinto(s), self.span.sinto(s))
    }
}
