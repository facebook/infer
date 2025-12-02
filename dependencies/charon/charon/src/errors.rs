//! Utilities to generate error reports about the external dependencies.
use crate::ast::*;
use crate::formatter::IntoFormatter;
use crate::pretty::FmtWithCtx;
pub use annotate_snippets::Level;
use itertools::Itertools;
use macros::VariantIndexArity;
use petgraph::algo::dijkstra::dijkstra;
use petgraph::prelude::DiGraphMap;
use std::cmp::{Ord, PartialOrd};
use std::collections::{HashMap, HashSet};

#[macro_export]
macro_rules! register_error {
    ($ctx:expr, crate($krate:expr), $span: expr, $($fmt:tt)*) => {{
        let msg = format!($($fmt)*);
        $ctx.span_err($krate, $span, &msg, $crate::errors::Level::WARNING)
    }};
    ($ctx:expr, $span: expr, $($fmt:tt)*) => {{
        let msg = format!($($fmt)*);
        $ctx.span_err($span, &msg, $crate::errors::Level::WARNING)
    }};
}
pub use register_error;

/// Macro to either panic or return on error, depending on the CLI options
#[macro_export]
macro_rules! raise_error {
    ($($tokens:tt)*) => {{
        return Err(register_error!($($tokens)*));
    }};
}
pub use raise_error;

/// Custom assert to either panic or return an error
#[macro_export]
macro_rules! error_assert {
    ($ctx:expr, $span: expr, $b: expr) => {
        if !$b {
            $crate::errors::raise_error!($ctx, $span, "assertion failure: {:?}", stringify!($b));
        }
    };
    ($ctx:expr, $span: expr, $b: expr, $($fmt:tt)*) => {
        if !$b {
            $crate::errors::raise_error!($ctx, $span, $($fmt)*);
        }
    };
}
pub use error_assert;

/// Custom assert to report an error and optionally panic
#[macro_export]
macro_rules! sanity_check {
    ($ctx:expr, $span: expr, $b: expr) => {
        if !$b {
            $crate::errors::register_error!(
                $ctx,
                $span,
                "assertion failure: {:?}",
                stringify!($b)
            );
        }
    };
    ($ctx:expr, $span: expr, $b: expr, $($fmt:tt)*) => {
        if !$b {
            $crate::errors::register_error!($ctx, $span, $($fmt)*);
        }
    };
}
pub use sanity_check;

/// Common error used during the translation.
#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub msg: String,
}

impl Error {
    pub(crate) fn render(&self, krate: &TranslatedCrate, level: Level) -> String {
        use annotate_snippets::*;
        let span = self.span.span;

        let mut group = Group::with_title(level.title(&self.msg));
        let origin;
        if let Some(file) = krate.files.get(span.file_id) {
            origin = format!("{}", file.name);
            if let Some(source) = &file.contents {
                let snippet = Snippet::source(source)
                    .path(&origin)
                    .fold(true)
                    .annotation(AnnotationKind::Primary.span(span.to_byte_range(source)));
                group = group.element(snippet);
            } else {
                // Show just the file and line/col.
                let origin = Origin::path(&origin)
                    .line(span.beg.line)
                    .char_column(span.beg.col + 1)
                    .primary(true);
                group = group.element(origin);
            }
        }

        Renderer::styled().render(&[group]).to_string()
    }
}

/// Display an error without a specific location.
pub fn display_unspanned_error(level: Level, msg: &str) {
    use annotate_snippets::*;
    let title = level.title(msg);
    let message = Renderer::styled()
        .render(&[Group::with_title(title)])
        .to_string();
    anstream::eprintln!("{message}\n");
}

/// We use this to save the origin of an id. This is useful for the external
/// dependencies, especially if some external dependencies don't extract:
/// we use this information to tell the user what is the code which
/// (transitively) lead to the extraction of those problematic dependencies.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DepSource {
    pub src_id: AnyTransId,
    /// The location where the id was referred to. We store `None` for external dependencies as we
    /// don't want to show these to the users.
    pub span: Option<Span>,
}

/// For tracing error dependencies.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, VariantIndexArity)]
enum DepNode {
    External(AnyTransId),
    /// We use the span information only for local references
    Local(AnyTransId, Span),
}

/// Graph of dependencies between erroring definitions and the definitions they came from.
struct DepGraph {
    dgraph: DiGraphMap<DepNode, ()>,
}

impl DepGraph {
    fn new() -> Self {
        DepGraph {
            dgraph: DiGraphMap::new(),
        }
    }

    fn insert_node(&mut self, n: DepNode) {
        // We have to be careful about duplicate nodes
        if !self.dgraph.contains_node(n) {
            self.dgraph.add_node(n);
        }
    }

    fn insert_edge(&mut self, from: DepNode, to: DepNode) {
        self.insert_node(from);
        self.insert_node(to);
        if !self.dgraph.contains_edge(from, to) {
            self.dgraph.add_edge(from, to, ());
        }
    }
}

impl std::fmt::Display for DepGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        for (from, to, _) in self.dgraph.all_edges() {
            writeln!(f, "{from:?} -> {to:?}")?
        }
        Ok(())
    }
}

/// The context for tracking and reporting errors.
pub struct ErrorCtx {
    /// If true, do not abort on the first error and attempt to extract as much as possible.
    pub continue_on_failure: bool,
    /// If true, print the warnings as errors, and abort if any errors were raised.
    pub error_on_warnings: bool,

    /// The ids of the external_declarations for which extraction we encountered errors.
    pub external_decls_with_errors: HashSet<AnyTransId>,
    /// Graph of dependencies between items: there is an edge from item `a` to item `b` if `b`
    /// registered the id for `a` during its translation. Because we only use this to report errors
    /// on external items, we only record edges where `a` is an external item.
    external_dep_graph: DepGraph,
    /// The id of the definition we are exploring, used to track the source of errors.
    pub def_id: Option<AnyTransId>,
    /// Whether the definition being explored is local to the crate or not.
    pub def_id_is_local: bool,
    /// The number of errors encountered so far.
    pub error_count: usize,
}

impl ErrorCtx {
    pub fn new(continue_on_failure: bool, error_on_warnings: bool) -> Self {
        Self {
            continue_on_failure,
            error_on_warnings,
            external_decls_with_errors: HashSet::new(),
            external_dep_graph: DepGraph::new(),
            def_id: None,
            def_id_is_local: false,
            error_count: 0,
        }
    }

    pub fn continue_on_failure(&self) -> bool {
        self.continue_on_failure
    }
    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    /// Report an error without registering anything.
    pub fn display_error(
        &self,
        krate: &TranslatedCrate,
        span: Span,
        level: Level,
        msg: String,
    ) -> Error {
        let error = Error { span, msg };
        anstream::eprintln!("{}\n", error.render(krate, level));
        error
    }

    /// Report and register an error.
    pub fn span_err(
        &mut self,
        krate: &TranslatedCrate,
        span: Span,
        msg: &str,
        level: Level,
    ) -> Error {
        let level = if level == Level::WARNING && self.error_on_warnings {
            Level::ERROR
        } else {
            level
        };
        let err = self.display_error(krate, span, level, msg.to_string());
        self.error_count += 1;
        // If this item comes from an external crate, after the first error for that item we
        // display where in the local crate that item was reached from.
        if !self.def_id_is_local
            && let Some(id) = self.def_id
            && self.external_decls_with_errors.insert(id)
        {
            self.report_external_dep_error(krate, id);
        }
        if !self.continue_on_failure() {
            panic!("{msg}");
        }
        err
    }

    /// Register the fact that `id` is a dependency of `src` (if `src` is not `None`).
    pub fn register_dep_source(
        &mut self,
        src: &Option<DepSource>,
        item_id: AnyTransId,
        is_local: bool,
    ) {
        if let Some(src) = src
            && src.src_id != item_id
            && !is_local
        {
            let src_node = DepNode::External(item_id);
            self.external_dep_graph.insert_node(src_node);

            let tgt_node = match src.span {
                Some(span) => DepNode::Local(src.src_id, span),
                None => DepNode::External(src.src_id),
            };
            self.external_dep_graph.insert_edge(src_node, tgt_node)
        }
    }

    /// In case errors happened when extracting the definitions coming from the external
    /// dependencies, print a detailed report to explain to the user which dependencies were
    /// problematic, and where they are used in the code.
    pub fn report_external_dep_error(&self, krate: &TranslatedCrate, id: AnyTransId) {
        use annotate_snippets::*;

        // Use `Dijkstra's` algorithm to find the local items reachable from the current non-local
        // item.
        let graph = &self.external_dep_graph;
        let reachable = dijkstra(&graph.dgraph, DepNode::External(id), None, |_| 1);
        trace!("id: {:?}\nreachable:\n{:?}", id, reachable);

        // Collect reachable local spans.
        let by_file: HashMap<FileId, Vec<Span>> = reachable
            .iter()
            .filter_map(|(n, _)| match n {
                DepNode::External(_) => None,
                DepNode::Local(_, span) => Some(*span),
            })
            .into_group_map_by(|span| span.span.file_id);

        // Collect to a `Vec` to be able to sort it and to borrow `origin` (needed by
        // `Snippet::source`).
        let mut by_file: Vec<(FileId, _, _, Vec<Span>)> = by_file
            .into_iter()
            .filter_map(|(file_id, mut spans)| {
                spans.sort(); // Sort spans to display in file order
                let file = krate.files.get(file_id)?;
                let source = file.contents.as_ref()?;
                let file_name = file.name.to_string();
                Some((file_id, file_name, source, spans))
            })
            .collect();
        // Sort by file id to avoid output instability.
        by_file.sort_by_key(|(file_id, ..)| *file_id);

        let level = Level::NOTE;
        let snippets = by_file.iter().map(|(_, origin, source, spans)| {
            Snippet::source(*source)
                .path(origin)
                .fold(true)
                .annotations(
                    spans
                        .iter()
                        .map(|span| AnnotationKind::Context.span(span.span.to_byte_range(source))),
                )
        });

        let msg = format!(
            "the error occurred when translating `{}`, \
             which is (transitively) used at the following location(s):",
            id.with_ctx(&krate.into_fmt())
        );
        let message = Group::with_title(level.title(&msg)).elements(snippets);
        let out = Renderer::styled().render(&[message]).to_string();
        anstream::eprintln!("{}", out);
    }
}
