//! Translate information about items: name, attributes, etc.
use super::translate_crate::RustcItem;
use super::translate_ctx::*;
use super::translate_generics::BindingLevel;
use charon_lib::ast::*;
use hax_frontend_exporter::{self as hax, DefPathItem};
use itertools::Itertools;
use std::cmp::Ord;
use std::path::{Component, PathBuf};

// Spans
impl<'tcx, 'ctx> TranslateCtx<'tcx> {
    /// Register a file if it is a "real" file and was not already registered
    /// `span` must be a span from which we obtained that filename.
    fn register_file(&mut self, filename: FileName, span: rustc_span::Span) -> FileId {
        // Lookup the file if it was already registered
        match self.file_to_id.get(&filename) {
            Some(id) => *id,
            None => {
                let source_file = self.tcx.sess.source_map().lookup_source_file(span.lo());
                let crate_name = self.tcx.crate_name(source_file.cnum).to_string();
                let file = File {
                    name: filename.clone(),
                    crate_name,
                    contents: source_file.src.as_deref().cloned(),
                };
                let id = self.translated.files.push(file);
                self.file_to_id.insert(filename, id);
                id
            }
        }
    }

    pub fn translate_filename(&mut self, name: &hax::FileName) -> meta::FileName {
        match name {
            hax::FileName::Real(name) => {
                use hax::RealFileName;
                match name {
                    RealFileName::LocalPath(path) => {
                        let path = if let Ok(path) = path.strip_prefix(&self.sysroot) {
                            // The path to files in the standard library may be full paths to somewhere
                            // in the sysroot. This may depend on how the toolchain is installed
                            // (rustup vs nix), so we normalize the paths here to avoid
                            // inconsistencies in the translation.
                            if let Ok(path) = path.strip_prefix("lib/rustlib/src/rust") {
                                let mut rewritten_path: PathBuf = "/rustc".into();
                                rewritten_path.extend(path);
                                rewritten_path
                            } else {
                                // Unclear if this can happen, but just in case.
                                let mut rewritten_path: PathBuf = "/toolchain".into();
                                rewritten_path.extend(path);
                                rewritten_path
                            }
                        } else {
                            path.clone()
                        };
                        FileName::Local(path)
                    }
                    RealFileName::Remapped { virtual_name, .. } => {
                        // We use the virtual name because it is always available.
                        // That name normally starts with `/rustc/<hash>/`. For our purposes we hide
                        // the hash.
                        let mut components_iter = virtual_name.components();
                        if let Some(
                            [
                                Component::RootDir,
                                Component::Normal(rustc),
                                Component::Normal(hash),
                            ],
                        ) = components_iter.by_ref().array_chunks().next()
                            && rustc.to_str() == Some("rustc")
                            && hash.len() == 40
                        {
                            let path_without_hash = [Component::RootDir, Component::Normal(rustc)]
                                .into_iter()
                                .chain(components_iter)
                                .collect();
                            FileName::Virtual(path_without_hash)
                        } else {
                            FileName::Virtual(virtual_name.clone())
                        }
                    }
                }
            }
            // We use the debug formatter to generate a filename.
            // This is not ideal, but filenames are for debugging anyway.
            _ => FileName::NotReal(format!("{name:?}")),
        }
    }

    pub fn translate_raw_span(&mut self, rspan: &hax::Span) -> meta::RawSpan {
        let filename = self.translate_filename(&rspan.filename);
        let rust_span = rspan.rust_span_data.unwrap().span();
        let file_id = match &filename {
            FileName::NotReal(_) => {
                // For now we forbid not real filenames
                unimplemented!();
            }
            FileName::Virtual(_) | FileName::Local(_) => self.register_file(filename, rust_span),
        };

        fn convert_loc(loc: &hax::Loc) -> Loc {
            Loc {
                line: loc.line,
                col: loc.col,
            }
        }
        let beg = convert_loc(&rspan.lo);
        let end = convert_loc(&rspan.hi);

        // Put together
        meta::RawSpan { file_id, beg, end }
    }

    /// Compute span data from a Rust source scope
    pub fn translate_span_from_source_info(
        &mut self,
        source_scopes: &hax::IndexVec<hax::SourceScope, hax::SourceScopeData>,
        source_info: &hax::SourceInfo,
    ) -> Span {
        // Translate the span
        let span = self.translate_raw_span(&source_info.span);

        // Lookup the top-most inlined parent scope.
        let mut parent_span = None;
        let mut scope_data = &source_scopes[source_info.scope];
        while let Some(parent_scope) = scope_data.inlined_parent_scope {
            scope_data = &source_scopes[parent_scope];
            parent_span = Some(&scope_data.span);
        }

        if let Some(parent_span) = parent_span {
            let parent_span = self.translate_raw_span(parent_span);
            Span {
                span: parent_span,
                generated_from_span: Some(span),
            }
        } else {
            Span {
                span,
                generated_from_span: None,
            }
        }
    }

    pub(crate) fn translate_span_from_hax(&mut self, span: &hax::Span) -> Span {
        Span {
            span: self.translate_raw_span(span),
            generated_from_span: None,
        }
    }

    pub(crate) fn def_span(&mut self, def_id: &hax::DefId) -> Span {
        let span = def_id.def_span(&self.hax_state);
        self.translate_span_from_hax(&span)
    }
}

// Names
impl<'tcx, 'ctx> TranslateCtx<'tcx> {
    fn path_elem_for_def(
        &mut self,
        span: Span,
        item: &RustcItem,
    ) -> Result<Option<PathElem>, Error> {
        let def_id = item.def_id();
        let path_elem = def_id.path_item();
        // Disambiguator disambiguates identically-named (but distinct) identifiers. This happens
        // notably with macros and inherent impl blocks.
        let disambiguator = Disambiguator::new(path_elem.disambiguator as usize);
        // Match over the key data
        let path_elem = match path_elem.data {
            DefPathItem::CrateRoot { name, .. } => {
                // Sanity check
                error_assert!(self, span, path_elem.disambiguator == 0);
                Some(PathElem::Ident(name.clone(), disambiguator))
            }
            // We map the three namespaces onto a single one. We can always disambiguate by looking
            // at the definition.
            DefPathItem::TypeNs(symbol)
            | DefPathItem::ValueNs(symbol)
            | DefPathItem::MacroNs(symbol) => Some(PathElem::Ident(symbol, disambiguator)),
            DefPathItem::Impl => {
                let full_def = self.hax_def_for_item(item)?;
                // Two cases, depending on whether the impl block is
                // a "regular" impl block (`impl Foo { ... }`) or a trait
                // implementation (`impl Bar for Foo { ... }`).
                let impl_elem = match full_def.kind() {
                    // Inherent impl ("regular" impl)
                    hax::FullDefKind::InherentImpl { ty, .. } => {
                        // We need to convert the type, which may contain quantified
                        // substs and bounds. In order to properly do so, we introduce
                        // a body translation context.
                        let mut bt_ctx = ItemTransCtx::new(
                            TransItemSource::new(item.clone(), TransItemSourceKind::InherentImpl),
                            None,
                            self,
                        );
                        bt_ctx.translate_def_generics(span, &full_def)?;
                        let ty = bt_ctx.translate_ty(span, &ty)?;
                        ImplElem::Ty(Binder {
                            kind: BinderKind::InherentImplBlock,
                            params: bt_ctx.into_generics(),
                            skip_binder: ty,
                        })
                    }
                    // Trait implementation
                    hax::FullDefKind::TraitImpl { .. } => {
                        let impl_id = {
                            let item_src = TransItemSource::new(
                                item.clone(),
                                TransItemSourceKind::TraitImpl(TraitImplSource::Normal),
                            );
                            self.register_and_enqueue(&None, item_src).unwrap()
                        };
                        ImplElem::Trait(impl_id)
                    }
                    _ => unreachable!(),
                };

                Some(PathElem::Impl(impl_elem))
            }
            // TODO: do nothing for now
            DefPathItem::OpaqueTy => None,
            // TODO: this is not very satisfactory, but on the other hand
            // we should be able to extract closures in local let-bindings
            // (i.e., we shouldn't have to introduce top-level let-bindings).
            DefPathItem::Closure => Some(PathElem::Ident("closure".to_string(), disambiguator)),
            // Do nothing, functions in `extern` blocks are in the same namespace as the
            // block.
            DefPathItem::ForeignMod => None,
            // Do nothing, the constructor of a struct/variant has the same name as the
            // struct/variant.
            DefPathItem::Ctor => None,
            DefPathItem::Use => Some(PathElem::Ident("{use}".to_string(), disambiguator)),
            DefPathItem::AnonConst => Some(PathElem::Ident("{const}".to_string(), disambiguator)),
            DefPathItem::PromotedConst => Some(PathElem::Ident(
                "{promoted_const}".to_string(),
                disambiguator,
            )),
            _ => {
                raise_error!(
                    self,
                    span,
                    "Unexpected DefPathItem for `{def_id:?}`: {path_elem:?}"
                );
            }
        };
        Ok(path_elem)
    }

    /// Retrieve the name for this [`hax::DefId`]. Because a given `DefId` may give rise to several
    /// charon items, prefer to use `translate_name` when possible.
    ///
    /// We lookup the path associated to an id, and convert it to a name.
    /// Paths very precisely identify where an item is. There are important
    /// subcases, like the items in an `Impl` block:
    /// ```ignore
    /// impl<T> List<T> {
    ///   fn new() ...
    /// }
    /// ```
    ///
    /// One issue here is that "List" *doesn't appear* in the path, which would
    /// look like the following:
    ///
    ///   `TypeNS("Crate") :: Impl :: ValueNs("new")`
    ///                       ^^^
    ///           This is where "List" should be
    ///
    /// For this reason, whenever we find an `Impl` path element, we actually
    /// lookup the type of the sub-path, from which we can derive a name.
    ///
    /// Besides, as there may be several "impl" blocks for one type, each impl
    /// block is identified by a unique number (rustc calls this a
    /// "disambiguator"), which we grab.
    ///
    /// Example:
    /// ========
    /// For instance, if we write the following code in crate `test` and module
    /// `bla`:
    /// ```ignore
    /// impl<T> Foo<T> {
    ///   fn foo() { ... }
    /// }
    ///
    /// impl<T> Foo<T> {
    ///   fn bar() { ... }
    /// }
    /// ```
    ///
    /// The names we will generate for `foo` and `bar` are:
    /// `[Ident("test"), Ident("bla"), Ident("Foo"), Impl(impl<T> Ty<T>, Disambiguator(0)), Ident("foo")]`
    /// `[Ident("test"), Ident("bla"), Ident("Foo"), Impl(impl<T> Ty<T>, Disambiguator(1)), Ident("bar")]`
    fn name_for_item(&mut self, item: &RustcItem) -> Result<Name, Error> {
        if let Some(name) = self.cached_names.get(item) {
            return Ok(name.clone());
        }
        let def_id = item.def_id();
        trace!("Computing name for `{def_id:?}`");

        let parent_name = if let Some(parent_id) = &def_id.parent {
            let def = self.hax_def_for_item(item)?;
            if matches!(item, RustcItem::Mono(..))
                && let Some(parent_item) = def.typing_parent(&self.hax_state)
            {
                self.name_for_item(&RustcItem::Mono(parent_item.clone()))?
            } else {
                self.name_for_item(&RustcItem::Poly(parent_id.clone()))?
            }
        } else {
            Name { name: Vec::new() }
        };
        let span = self.def_span(def_id);
        let mut name = parent_name;
        if let Some(path_elem) = self.path_elem_for_def(span, item)? {
            name.name.push(path_elem);
        }

        trace!("Computed name for `{def_id:?}`: `{name:?}`");
        self.cached_names.insert(item.clone(), name.clone());
        Ok(name)
    }

    /// Compute the name for an item.
    /// Internal function, use `translate_name`.
    pub fn name_for_src(&mut self, src: &TransItemSource) -> Result<Name, Error> {
        let mut name = if let Some(parent) = src.parent() {
            self.name_for_src(&parent)?
        } else {
            self.name_for_item(&src.item)?
        };
        match &src.kind {
            TransItemSourceKind::TraitImpl(
                kind @ (TraitImplSource::Closure(..)
                | TraitImplSource::DropGlue
                | TraitImplSource::TraitAlias),
            ) => {
                if let TraitImplSource::Closure(..) = kind {
                    let _ = name.name.pop(); // Pop the `{closure}`
                }
                let impl_id = self.register_and_enqueue(&None, src.clone()).unwrap();
                name.name.push(PathElem::Impl(ImplElem::Trait(impl_id)));
            }
            TransItemSourceKind::ClosureMethod(kind) => {
                let fn_name = kind.method_name().to_string();
                name.name
                    .push(PathElem::Ident(fn_name, Disambiguator::ZERO));
            }
            TransItemSourceKind::DropGlueMethod => {
                name.name
                    .push(PathElem::Ident("drop".to_string(), Disambiguator::ZERO));
            }
            TransItemSourceKind::ClosureAsFnCast => {
                name.name
                    .push(PathElem::Ident("as_fn".into(), Disambiguator::ZERO));
            }
            TransItemSourceKind::VTable
            | TransItemSourceKind::VTableInstance(..)
            | TransItemSourceKind::VTableInstanceInitializer(..) => {
                name.name
                    .push(PathElem::Ident("{vtable}".into(), Disambiguator::ZERO));
            }
            _ => {}
        }
        Ok(name)
    }

    /// Retrieve the name for an item.
    pub fn translate_name(&mut self, src: &TransItemSource) -> Result<Name, Error> {
        let mut name = self.name_for_src(src)?;
        // Push the generics used for monomorphization, if any.
        if let RustcItem::Mono(item_ref) = &src.item
            && !item_ref.generic_args.is_empty()
        {
            let trans_id = self.register_no_enqueue(&None, src).unwrap();
            let span = self.def_span(&item_ref.def_id);
            let mut bt_ctx = ItemTransCtx::new(src.clone(), trans_id, self);
            bt_ctx.binding_levels.push(BindingLevel::new(true));
            let args = bt_ctx.translate_generic_args(
                span,
                &item_ref.generic_args,
                &item_ref.impl_exprs,
            )?;
            name.name.push(PathElem::Monomorphized(Box::new(args)));
        }
        Ok(name)
    }

    /// Remark: this **doesn't** register the def id (on purpose)
    pub(crate) fn translate_trait_item_name(
        &mut self,
        def_id: &hax::DefId,
    ) -> Result<TraitItemName, Error> {
        let def = self.poly_hax_def(def_id)?;
        let assoc = match def.kind() {
            hax::FullDefKind::AssocTy {
                associated_item, ..
            }
            | hax::FullDefKind::AssocConst {
                associated_item, ..
            }
            | hax::FullDefKind::AssocFn {
                associated_item, ..
            } => associated_item,
            _ => panic!("Unexpected def for associated item: {def:?}"),
        };
        Ok(TraitItemName(assoc.name.clone().unwrap_or_default()))
    }

    pub(crate) fn opacity_for_name(&self, name: &Name) -> ItemOpacity {
        self.options.opacity_for_name(&self.translated, name)
    }
}

// Attributes
impl<'tcx, 'ctx> TranslateCtx<'tcx> {
    /// Translates a rust attribute. Returns `None` if the attribute is a doc comment (rustc
    /// encodes them as attributes). For now we use `String`s for `Attributes`.
    pub(crate) fn translate_attribute(&mut self, attr: &hax::Attribute) -> Option<Attribute> {
        use hax::Attribute as Haxttribute;
        use hax::AttributeKind as HaxttributeKind;
        match attr {
            Haxttribute::Parsed(HaxttributeKind::DocComment { comment, .. }) => {
                Some(Attribute::DocComment(comment.to_string()))
            }
            Haxttribute::Parsed(_) => None,
            Haxttribute::Unparsed(attr) => {
                let raw_attr = RawAttribute {
                    path: attr.path.clone(),
                    args: match &attr.args {
                        hax::AttrArgs::Empty => None,
                        hax::AttrArgs::Delimited(args) => Some(args.tokens.clone()),
                        hax::AttrArgs::Eq { expr, .. } => self
                            .tcx
                            .sess
                            .source_map()
                            .span_to_snippet(expr.span.rust_span_data.unwrap().span())
                            .ok(),
                    },
                };
                match Attribute::parse_from_raw(raw_attr) {
                    Ok(a) => Some(a),
                    Err(msg) => {
                        let span = self.translate_span_from_hax(&attr.span);
                        register_error!(self, span, "Error parsing attribute: {msg}");
                        None
                    }
                }
            }
        }
    }

    pub(crate) fn translate_inline(&self, def: &hax::FullDef) -> Option<InlineAttr> {
        match def.kind() {
            hax::FullDefKind::Fn { inline, .. } | hax::FullDefKind::AssocFn { inline, .. } => {
                match inline {
                    hax::InlineAttr::None => None,
                    hax::InlineAttr::Hint => Some(InlineAttr::Hint),
                    hax::InlineAttr::Never => Some(InlineAttr::Never),
                    hax::InlineAttr::Always => Some(InlineAttr::Always),
                    hax::InlineAttr::Force { .. } => Some(InlineAttr::Always),
                }
            }
            _ => None,
        }
    }

    pub(crate) fn translate_attr_info(&mut self, def: &hax::FullDef) -> AttrInfo {
        // Default to `false` for impl blocks and closures.
        let public = def.visibility.unwrap_or(false);
        let inline = self.translate_inline(def);
        let attributes = def
            .attributes
            .iter()
            .filter_map(|attr| self.translate_attribute(&attr))
            .collect_vec();

        let rename = {
            let mut renames = attributes.iter().filter_map(|a| a.as_rename()).cloned();
            let rename = renames.next();
            if renames.next().is_some() {
                let span = self.translate_span_from_hax(&def.span);
                register_error!(
                    self,
                    span,
                    "There should be at most one `charon::rename(\"...\")` \
                    or `aeneas::rename(\"...\")` attribute per declaration",
                );
            }
            rename
        };

        AttrInfo {
            attributes,
            inline,
            public,
            rename,
        }
    }
}

// `ItemMeta`
impl<'tcx, 'ctx> TranslateCtx<'tcx> {
    /// Whether this item is in an `extern { .. }` block, in which case it has no body.
    pub(crate) fn is_extern_item(&mut self, def: &hax::FullDef) -> bool {
        def.def_id()
            .parent
            .as_ref()
            .is_some_and(|parent| matches!(parent.kind, hax::DefKind::ForeignMod { .. }))
    }

    /// Compute the meta information for a Rust item.
    pub(crate) fn translate_item_meta(
        &mut self,
        def: &hax::FullDef,
        item_src: &TransItemSource,
        name: Name,
        name_opacity: ItemOpacity,
    ) -> ItemMeta {
        if let Some(item_meta) = self.cached_item_metas.get(&item_src) {
            return item_meta.clone();
        }
        let span = def.source_span.as_ref().unwrap_or(&def.span);
        let span = self.translate_span_from_hax(span);
        let is_local = def.def_id().is_local;
        let (attr_info, lang_item) = if item_src.is_derived_item() {
            (AttrInfo::default(), None)
        } else {
            let attr_info = self.translate_attr_info(def);
            let lang_item = def
                .lang_item
                .clone()
                .or_else(|| def.diagnostic_item.clone());
            (attr_info, lang_item)
        };

        let opacity = if self.is_extern_item(def)
            || attr_info.attributes.iter().any(|attr| attr.is_opaque())
        {
            // Force opaque in these cases.
            ItemOpacity::Opaque.max(name_opacity)
        } else {
            name_opacity
        };

        let item_meta = ItemMeta {
            name,
            span,
            source_text: def.source_text.clone(),
            attr_info,
            is_local,
            opacity,
            lang_item,
        };
        self.cached_item_metas
            .insert(item_src.clone(), item_meta.clone());
        item_meta
    }
}

impl<'tcx, 'ctx> ItemTransCtx<'tcx, 'ctx> {
    pub(crate) fn translate_span_from_hax(&mut self, rspan: &hax::Span) -> Span {
        self.t_ctx.translate_span_from_hax(rspan)
    }

    pub(crate) fn def_span(&mut self, def_id: &hax::DefId) -> Span {
        self.t_ctx.def_span(def_id)
    }
}
