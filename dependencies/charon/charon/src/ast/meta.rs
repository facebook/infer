//! Meta-information about programs (spans, etc.).

pub use super::meta_utils::*;
use crate::names::Name;
use derive_generic_visitor::{Drive, DriveMut};
use macros::{EnumAsGetters, EnumIsA, EnumToGetters};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

generate_index_type!(FileId);

#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
pub struct Loc {
    /// The (1-based) line number.
    pub line: usize,
    /// The (0-based) column offset.
    pub col: usize,
}

/// Span information
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Drive, DriveMut)]
pub struct RawSpan {
    #[charon::rename("file")]
    pub file_id: FileId,
    #[charon::rename("beg_loc")]
    pub beg: Loc,
    #[charon::rename("end_loc")]
    pub end: Loc,
}

/// Meta information about a piece of code (block, statement, etc.)
#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
#[drive(skip)]
pub struct Span {
    /// The source code span.
    ///
    /// If this meta information is for a statement/terminator coming from a macro
    /// expansion/inlining/etc., this span is (in case of macros) for the macro
    /// before expansion (i.e., the location the code where the user wrote the call
    /// to the macro).
    ///
    /// Ex:
    /// ```text
    /// // Below, we consider the spans for the statements inside `test`
    ///
    /// //   the statement we consider, which gets inlined in `test`
    ///                          VV
    /// macro_rules! macro { ... st ... } // `generated_from_span` refers to this location
    ///
    /// fn test() {
    ///     macro!(); // <-- `span` refers to this location
    /// }
    /// ```
    pub span: RawSpan,
    /// Where the code actually comes from, in case of macro expansion/inlining/etc.
    pub generated_from_span: Option<RawSpan>,
}

/// `#[inline]` built-in attribute.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize, Drive, DriveMut)]
pub enum InlineAttr {
    /// `#[inline]`
    Hint,
    /// `#[inline(never)]`
    Never,
    /// `#[inline(always)]`
    Always,
}

/// Attributes (`#[...]`).
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    EnumIsA,
    EnumAsGetters,
    EnumToGetters,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
)]
#[charon::variants_prefix("Attr")]
pub enum Attribute {
    /// Do not translate the body of this item.
    /// Written `#[charon::opaque]`
    Opaque,
    /// Provide a new name that consumers of the llbc can use.
    /// Written `#[charon::rename("new_name")]`
    Rename(String),
    /// For enums only: rename the variants by pre-pending their names with the given prefix.
    /// Written `#[charon::variants_prefix("prefix_")]`.
    VariantsPrefix(String),
    /// Same as `VariantsPrefix`, but appends to the name instead of pre-pending.
    VariantsSuffix(String),
    /// A doc-comment such as `/// ...`.
    DocComment(String),
    /// A non-charon-specific attribute.
    Unknown(RawAttribute),
}

/// A general attribute.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Drive, DriveMut)]
pub struct RawAttribute {
    pub path: String,
    /// The arguments passed to the attribute, if any. We don't distinguish different delimiters or
    /// the `path = lit` case.
    pub args: Option<String>,
}

/// Information about the attributes and visibility of an item, field or variant..
#[derive(Debug, Default, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct AttrInfo {
    /// Attributes (`#[...]`).
    pub attributes: Vec<Attribute>,
    /// Inline hints (on functions only).
    pub inline: Option<InlineAttr>,
    /// The name computed from `charon::rename` and `charon::variants_prefix` attributes, if any.
    /// This provides a custom name that can be used by consumers of llbc. E.g. Aeneas uses this to
    /// rename definitions in the extracted code.
    pub rename: Option<String>,
    /// Whether this item is declared public. Impl blocks and closures don't have visibility
    /// modifiers; we arbitrarily set this to `false` for them.
    ///
    /// Note that this is different from being part of the crate's public API: to be part of the
    /// public API, an item has to also be reachable from public items in the crate root. For
    /// example:
    /// ```rust,ignore
    /// mod foo {
    ///     pub struct X;
    /// }
    /// mod bar {
    ///     pub fn something(_x: super::foo::X) {}
    /// }
    /// pub use bar::something; // exposes `X`
    /// ```
    /// Without the `pub use ...`, neither `X` nor `something` would be part of the crate's public
    /// API (this is called "pub-in-priv" items). With or without the `pub use`, we set `public =
    /// true`; computing item reachability is harder.
    pub public: bool,
}

#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
    Drive,
    DriveMut,
    EnumIsA,
)]
pub enum ItemOpacity {
    /// Translate the item fully.
    Transparent,
    /// Translate the item depending on the normal rust visibility of its contents: for types, we
    /// translate fully if it is a struct with public fields or an enum; for other items this is
    /// equivalent to `Opaque`.
    Foreign,
    /// Translate the item name and signature, but not its contents. For function and globals, this
    /// means we don't translate the body (the code); for ADTs, this means we don't translate the
    /// fields/variants. For traits and trait impls, this doesn't change anything. For modules,
    /// this means we don't explore its contents (we still translate any of its items mentioned
    /// from somewhere else).
    ///
    /// This can happen either if the item was annotated with `#[charon::opaque]` or if it was
    /// declared opaque via a command-line argument.
    Opaque,
    /// Translate nothing of this item. The corresponding map will not have an entry for the
    /// `AnyTransId`. Useful when even the signature of the item causes errors.
    Invisible,
}

/// Meta information about an item (function, trait decl, trait impl, type decl, global).
#[derive(Debug, Clone, Serialize, Deserialize, Drive, DriveMut)]
pub struct ItemMeta {
    pub name: Name,
    pub span: Span,
    /// The source code that corresponds to this item.
    #[drive(skip)]
    pub source_text: Option<String>,
    /// Attributes and visibility.
    #[drive(skip)]
    pub attr_info: AttrInfo,
    /// `true` if the type decl is a local type decl, `false` if it comes from an external crate.
    #[drive(skip)]
    pub is_local: bool,
    /// Whether this item is considered opaque. For function and globals, this means we don't
    /// translate the body (the code); for ADTs, this means we don't translate the fields/variants.
    /// For traits and trait impls, this doesn't change anything. For modules, this means we don't
    /// explore its contents (we still translate any of its items mentioned from somewhere else).
    ///
    /// This can happen either if the item was annotated with `#[charon::opaque]` or if it was
    /// declared opaque via a command-line argument.
    #[charon::opaque]
    #[drive(skip)]
    pub opacity: ItemOpacity,
    /// If the item is built-in, record its internal builtin identifier.
    #[drive(skip)]
    pub lang_item: Option<String>,
}

/// A filename.
#[derive(
    Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord, Serialize, Deserialize, Drive, DriveMut,
)]
pub enum FileName {
    /// A remapped path (namely paths into stdlib)
    #[drive(skip)] // drive is not implemented for `PathBuf`
    Virtual(PathBuf),
    /// A local path (a file coming from the current crate for instance)
    #[drive(skip)] // drive is not implemented for `PathBuf`
    Local(PathBuf),
    /// A "not real" file name (macro, query, etc.)
    #[charon::opaque]
    NotReal(String),
}

#[derive(
    Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord, Serialize, Deserialize, Drive, DriveMut,
)]
pub struct File {
    /// The path to the file.
    pub name: FileName,
    /// Name of the crate this file comes from.
    pub crate_name: String,
    /// The contents of the source file, as seen by rustc at the time of translation.
    /// Some files don't have contents.
    pub contents: Option<String>,
}
