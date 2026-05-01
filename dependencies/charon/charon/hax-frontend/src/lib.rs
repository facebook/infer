#![allow(rustdoc::private_intra_doc_links)]
#![feature(if_let_guard)]
#![feature(macro_metavar_expr)]
#![feature(rustc_private)]
#![feature(sized_hierarchy)]
#![feature(trait_alias)]
#![feature(type_changing_struct_update)]

extern crate rustc_abi;
extern crate rustc_apfloat;
extern crate rustc_ast;
extern crate rustc_ast_pretty;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hashes;
extern crate rustc_hir;
extern crate rustc_hir_analysis;
extern crate rustc_index;
extern crate rustc_infer;
extern crate rustc_interface;
extern crate rustc_lexer;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_trait_selection;
extern crate rustc_type_ir;

mod rustc_utils;
pub mod state;
mod utils;

mod constant_utils;
pub mod id_table;
mod types;

mod index_vec;
mod prelude;

pub use prelude::*;

mod sinto;
mod traits;

pub use hax_adt_into::AdtInto;
pub use sinto::SInto;

pub mod options {
    #[derive(Debug, Clone)]
    pub enum Glob {
        One,  // *
        Many, // **
    }

    impl ToString for Glob {
        fn to_string(&self) -> String {
            match self {
                Self::One => "*",
                Self::Many => "**",
            }
            .to_string()
        }
    }

    #[derive(Debug, Clone)]
    pub enum NamespaceChunk {
        Glob(Glob),
        Exact(String),
    }

    impl ToString for NamespaceChunk {
        fn to_string(&self) -> String {
            match self {
                Self::Glob(glob) => glob.to_string(),
                Self::Exact(string) => string.to_string(),
            }
        }
    }

    impl std::convert::From<&str> for NamespaceChunk {
        fn from(s: &str) -> Self {
            match s {
                "*" => NamespaceChunk::Glob(Glob::One),
                "**" => NamespaceChunk::Glob(Glob::Many),
                _ => NamespaceChunk::Exact(String::from(s)),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Namespace {
        pub chunks: Vec<NamespaceChunk>,
    }

    impl ToString for Namespace {
        fn to_string(&self) -> String {
            self.chunks
                .iter()
                .map(NamespaceChunk::to_string)
                .collect::<Vec<_>>()
                .join("::")
                .to_string()
        }
    }

    impl std::convert::From<String> for Namespace {
        fn from(s: String) -> Self {
            Namespace {
                chunks: s
                    .split("::")
                    .filter(|s| !s.is_empty())
                    .map(NamespaceChunk::from)
                    .collect(),
            }
        }
    }

    impl Namespace {
        pub fn matches(&self, path: &Vec<String>) -> bool {
            fn aux(pattern: &[NamespaceChunk], path: &[String]) -> bool {
                match (pattern, path) {
                    ([], []) => true,
                    ([NamespaceChunk::Exact(x), pattern @ ..], [y, path @ ..]) => {
                        x == y && aux(pattern, path)
                    }
                    ([NamespaceChunk::Glob(Glob::One), pattern @ ..], [_, path @ ..]) => {
                        aux(pattern, path)
                    }
                    ([NamespaceChunk::Glob(Glob::Many), pattern @ ..], []) => aux(pattern, path),
                    (
                        [NamespaceChunk::Glob(Glob::Many), pattern_tl @ ..],
                        [_path_hd, path_tl @ ..],
                    ) => aux(pattern_tl, path) || aux(pattern, path_tl),
                    _ => false,
                }
            }
            aux(self.chunks.as_slice(), path.as_slice())
        }
    }

    #[derive(Debug, Clone)]
    pub struct Options {
        /// Whether we should evaluate and inline the value of anonymous constants (inline `const {}`
        /// blocks or advanced constant expressions as in `[T; N+1]`), or refer to them as
        /// `GlobalName`s.
        pub inline_anon_consts: bool,
        /// Options related to bounds.
        pub bounds_options: BoundsOptions,
        /// Resolve definition identifiers to their concrete impl counterpart when possible in `ItemRef::translate`.
        pub item_ref_use_concrete_impl: bool,
    }

    #[derive(Debug, Clone, Copy)]
    pub struct BoundsOptions {
        /// Add `T: Destruct` bounds to every type generic, so that we can build `ImplExpr`s to know
        /// what code is run on drop.
        pub resolve_destruct: bool,
        /// Prune `T: Sized` and `T: MetaSized` predicates.
        pub prune_sized: bool,
    }
}
