//! This file groups everything which is linked to implementations about [crate::meta]
use crate::meta::*;
use crate::names::{Disambiguator, Name, PathElem};
use itertools::Itertools;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::iter::Iterator;
use std::ops::Range;

/// Given a line number within a source file, get the byte of the start of the line. Obviously not
/// efficient to do many times, but this is used is diagnostic paths only. The line numer is
/// expected to be 1-based.
fn line_to_start_byte(source: &str, line_nbr: usize) -> usize {
    let mut cur_byte = 0;
    for (i, line) in source.split_inclusive('\n').enumerate() {
        if line_nbr == i + 1 {
            break;
        }
        cur_byte += line.len();
    }
    cur_byte
}

impl Loc {
    fn dummy() -> Self {
        Loc { line: 0, col: 0 }
    }

    fn min(l0: &Loc, l1: &Loc) -> Loc {
        match l0.line.cmp(&l1.line) {
            Ordering::Equal => Loc {
                line: l0.line,
                col: std::cmp::min(l0.col, l1.col),
            },
            Ordering::Less => *l0,
            Ordering::Greater => *l1,
        }
    }

    fn max(l0: &Loc, l1: &Loc) -> Loc {
        match l0.line.cmp(&l1.line) {
            Ordering::Equal => Loc {
                line: l0.line,
                col: std::cmp::max(l0.col, l1.col),
            },
            Ordering::Greater => *l0,
            Ordering::Less => *l1,
        }
    }

    pub fn to_byte(self, source: &str) -> usize {
        line_to_start_byte(source, self.line) + self.col
    }
}

impl RawSpan {
    pub fn dummy() -> Self {
        RawSpan {
            file_id: FileId::from_raw(0),
            beg: Loc::dummy(),
            end: Loc::dummy(),
        }
    }

    /// Value with which we order `RawSpans`s.
    fn sort_key(&self) -> impl Ord {
        (self.file_id, self.beg, self.end)
    }

    pub fn to_byte_range(self, source: &str) -> Range<usize> {
        self.beg.to_byte(source)..self.end.to_byte(source)
    }
}

/// Manual impls because `SpanData` is not orderable.
impl PartialOrd for RawSpan {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for RawSpan {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.sort_key().cmp(&other.sort_key())
    }
}

impl Span {
    pub fn dummy() -> Self {
        Span {
            span: RawSpan::dummy(),
            generated_from_span: None,
        }
    }
}

/// Combine some span information (useful when we need to compute the
/// span-information of, say, a sequence).
pub fn combine_span(m0: &Span, m1: &Span) -> Span {
    // Merge the spans
    if m0.span.file_id == m1.span.file_id {
        let span = RawSpan {
            file_id: m0.span.file_id,
            beg: Loc::min(&m0.span.beg, &m1.span.beg),
            end: Loc::max(&m0.span.end, &m1.span.end),
        };

        // We don't attempt to merge the "generated from" spans: they might
        // come from different files, and even if they come from the same files
        // they might come from different macros, etc.
        Span {
            span,
            generated_from_span: None,
        }
    } else {
        // It happens that the spans don't come from the same file. In this
        // situation, we just return the first span. TODO: improve this.
        *m0
    }
}

/// Combine all the span information in a slice.
pub fn combine_span_iter<'a, T: Iterator<Item = &'a Span>>(mut ms: T) -> Span {
    // The iterator should have a next element
    let mut mc: Span = *ms.next().unwrap();
    for m in ms {
        mc = combine_span(&mc, m);
    }

    mc
}

impl FileName {
    pub fn to_string(&self) -> Cow<'_, str> {
        match self {
            FileName::Virtual(path_buf) | FileName::Local(path_buf) => path_buf.to_string_lossy(),
            FileName::NotReal(path) => Cow::Borrowed(path),
        }
    }
}

impl Attribute {
    /// Parse a raw attribute to recognize our special `charon::*` and `aeneas::*` attributes.
    pub fn parse_from_raw(raw_attr: RawAttribute) -> Result<Self, String> {
        // If the attribute path has two components, the first of which is `charon` or `aeneas`, we
        // try to parse it. Otherwise we return `Unknown`.
        let path = raw_attr.path.split("::").collect_vec();
        let attr_name = if let &[path_start, attr_name] = path.as_slice()
            && (path_start == "charon" || path_start == "aeneas")
        {
            attr_name
        } else {
            return Ok(Self::Unknown(raw_attr));
        };

        match Self::parse_special_attr(attr_name, raw_attr.args.as_deref())? {
            Some(parsed) => Ok(parsed),
            None => Err(format!(
                "Unrecognized attribute: `{}`",
                raw_attr.to_string()
            )),
        }
    }

    /// Parse a `charon::*` or `aeneas::*` attribute.
    fn parse_special_attr(attr_name: &str, args: Option<&str>) -> Result<Option<Self>, String> {
        let parsed = match attr_name {
            // `#[charon::opaque]`
            "opaque" if args.is_none() => Self::Opaque,
            // `#[charon::rename("new_name")]`
            "rename" if let Some(attr) = args => {
                let Some(attr) = attr
                    .strip_prefix("\"")
                    .and_then(|attr| attr.strip_suffix("\""))
                else {
                    return Err(format!(
                        "the new name should be between quotes: `rename(\"{attr}\")`."
                    ));
                };

                if attr.is_empty() {
                    return Err(format!("attribute `rename` should not be empty"));
                }

                let first_char = attr.chars().nth(0).unwrap();
                let is_identifier = (first_char.is_alphabetic() || first_char == '_')
                    && attr.chars().all(|c| c.is_alphanumeric() || c == '_');
                if !is_identifier {
                    return Err(format!(
                        "attribute `rename` should contain a valid identifier"
                    ));
                }

                Self::Rename(attr.to_string())
            }
            // `#[charon::variants_prefix("T")]`
            "variants_prefix" if let Some(attr) = args => {
                let Some(attr) = attr
                    .strip_prefix("\"")
                    .and_then(|attr| attr.strip_suffix("\""))
                else {
                    return Err(format!(
                        "the name should be between quotes: `variants_prefix(\"{attr}\")`."
                    ));
                };

                Self::VariantsPrefix(attr.to_string())
            }
            // `#[charon::variants_suffix("T")]`
            "variants_suffix" if let Some(attr) = args => {
                let Some(attr) = attr
                    .strip_prefix("\"")
                    .and_then(|attr| attr.strip_suffix("\""))
                else {
                    return Err(format!(
                        "the name should be between quotes: `variants_suffix(\"{attr}\")`."
                    ));
                };

                Self::VariantsSuffix(attr.to_string())
            }
            _ => return Ok(None),
        };
        Ok(Some(parsed))
    }
}

impl ItemOpacity {
    pub fn with_content_visibility(self, contents_are_public: bool) -> Self {
        use ItemOpacity::*;
        match self {
            Invisible => Invisible,
            Transparent => Transparent,
            Foreign if contents_are_public => Transparent,
            Foreign => Opaque,
            Opaque => Opaque,
        }
    }

    pub fn with_private_contents(self) -> Self {
        self.with_content_visibility(false)
    }
}

impl ItemMeta {
    pub fn renamed_name(&self) -> Name {
        let mut name = self.name.clone();
        if let Some(rename) = self.attr_info.rename.clone() {
            *name.name.last_mut().unwrap() = PathElem::Ident(rename, Disambiguator::new(0));
        }
        name
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::dummy()
    }
}
