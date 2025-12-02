use std::{fmt, str::FromStr};

use itertools::Itertools;
use nom::{
    Parser,
    bytes::complete::{tag, take_while},
    character::complete::{multispace0, multispace1},
    combinator::{map_res, success},
    error::ParseError,
    multi::separated_list0,
    sequence::{delimited, preceded},
};
use nom_supreme::{ParserExt, error::ErrorTree};

use super::{PatElem, PatTy, Pattern};
use crate::ast::RefKind;

type ParseResult<'a, T> = nom::IResult<&'a str, T, ErrorTree<&'a str>>;

/// Extra methods on parsers.
trait ParserExtExt<I, O, E>: Parser<I, O, E> + Sized
where
    I: Clone,
    E: ParseError<I>,
{
    fn followed_by<F, O2>(self, suffix: F) -> impl Parser<I, O, E>
    where
        F: Parser<I, O2, E>,
    {
        self.terminated(suffix)
    }
}
impl<I, O, E, P> ParserExtExt<I, O, E> for P
where
    I: Clone,
    E: ParseError<I>,
    P: Parser<I, O, E>,
{
}

/// The entry point for this module: parses a string into a `Pattern`.
impl FromStr for Pattern {
    type Err = ErrorTree<String>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_pattern_complete(s)
    }
}

fn parse_pattern_complete(i: &str) -> Result<Pattern, ErrorTree<String>> {
    nom_supreme::final_parser::final_parser(parse_pattern)(i)
        .map_err(|e: ErrorTree<_>| e.map_locations(|s: &str| s.to_string()))
}

fn parse_pattern(i: &str) -> ParseResult<'_, Pattern> {
    separated_list0(tag("::").followed_by(multispace0), parse_pat_elem)
        .map(|elems| Pattern { elems })
        .parse(i)
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.elems.iter().format("::").fmt(f)
    }
}

impl fmt::Debug for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

fn parse_pat_elem(i: &str) -> ParseResult<'_, PatElem> {
    let parse_glob = tag("*").map(|_| PatElem::Glob);
    parse_glob
        .or(parse_impl_elem)
        .or(parse_simple_elem)
        .parse(i)
}

fn parse_simple_elem(i: &str) -> ParseResult<'_, PatElem> {
    let ident = take_while(|c: char| c.is_alphanumeric() || c == '_');
    let (i, ident) = ident.followed_by(multispace0).parse(i)?;
    if ident == "_" {
        success(PatElem::Glob).parse(i)
    } else {
        let args = delimited(
            tag("<").followed_by(multispace0),
            separated_list0(
                tag(",").followed_by(multispace0),
                parse_pat_ty.followed_by(multispace0),
            ),
            tag(">"),
        );
        args.opt()
            .map(|args| PatElem::Ident {
                name: ident.to_string(),
                generics: args.unwrap_or_default(),
                is_trait: false,
            })
            .parse(i)
    }
}

fn parse_impl_elem(i: &str) -> ParseResult<'_, PatElem> {
    let for_ty = preceded(
        tag("for").followed_by(multispace1),
        parse_pat_ty.followed_by(multispace0),
    );
    let impl_contents = parse_pattern.followed_by(multispace0).and(for_ty.opt());
    let impl_expr = tag("{").followed_by(multispace0).precedes(
        delimited(
            tag("impl").followed_by(multispace1.cut()).opt(),
            impl_contents,
            tag("}"),
        )
        .cut(),
    );
    map_res(impl_expr, |(mut pat, for_ty)| {
        if let Some(for_ty) = for_ty {
            let last_elem = pat
                .elems
                .last_mut()
                .ok_or_else(|| anyhow::anyhow!("trait path must be nonempty"))?;
            let PatElem::Ident {
                generics, is_trait, ..
            } = last_elem
            else {
                return Err(anyhow::anyhow!("trait path must end in an ident"));
            };
            // Set the type as the first generic arg.
            generics.insert(0, for_ty);
            *is_trait = true;
        }
        Ok(PatElem::Impl(pat.into()))
    })
    .parse(i)
}

impl fmt::Display for PatElem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PatElem::Ident {
                name,
                generics,
                is_trait,
            } => {
                write!(f, "{name}")?;
                let generics = generics.as_slice();
                let (ty, generics) = if let [ty, generics @ ..] = generics
                    && *is_trait
                {
                    (Some(ty), generics)
                } else {
                    (None, generics)
                };
                if !generics.is_empty() {
                    write!(f, "<{}>", generics.iter().format(", "))?;
                }
                if let Some(ty) = ty {
                    write!(f, " for {ty}")?;
                }
                Ok(())
            }
            PatElem::Impl(pat) => write!(f, "{{impl {pat}}}"),
            PatElem::Glob => write!(f, "_"),
        }
    }
}

fn parse_pat_ty(i: &str) -> ParseResult<'_, PatTy> {
    let mutability = tag("mut").followed_by(multispace0).opt().map(|mtbl| {
        if mtbl.is_some() {
            RefKind::Mut
        } else {
            RefKind::Shared
        }
    });
    tag("&")
        .followed_by(multispace0)
        .precedes(mutability.and(parse_pat_ty))
        .map(|(mtbl, ty)| PatTy::Ref(mtbl, ty.into()))
        .or(parse_pattern.map(PatTy::Pat))
        .parse(i)
}

impl fmt::Display for PatTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PatTy::Pat(p) => write!(f, "{p}"),
            PatTy::Ref(RefKind::Shared, ty) => write!(f, "&{ty}"),
            PatTy::Ref(RefKind::Mut, ty) => write!(f, "&mut {ty}"),
        }
    }
}

#[test]
fn test_roundtrip() {
    let idempotent_test_strings = [
        "crate::foo::bar",
        "blah::_",
        "blah::_foo",
        "a::b::Type",
        "a::b::Type<_, _>",
        "Clone",
        "usize",
        "foo::{impl Clone for usize}::clone",
        "foo::{impl Clone for &&usize}",
        "foo::{impl PartialEq<_> for Type<_, _>}",
        "foo::{impl PartialEq<usize> for Box<u8>}",
        "foo::{impl foo::Trait<core::option::Option<_>> for alloc::boxed::Box<_>}::method",
    ];
    let other_test_strings = [
        ("blah::*", "blah::_"),
        ("crate  ::  foo  ::bar ", "crate::foo::bar"),
        ("a::b::Type < _  ,  _ >", "a::b::Type<_, _>"),
        ("{ impl  Clone  for  usize }", "{impl Clone for usize}"),
        ("{Clone for usize}", "{impl Clone for usize}"),
    ];
    let failures = [
        "{implClone for usize}",
        "{impl Clone forusize}",
        "foo::{impl  for alloc::boxed::Box<_>}::method",
        "foo::{impl foo::_ for alloc::boxed::Box<_>}::method",
        "foo::{impl &Clone for usize}",
    ];

    let test_strings = idempotent_test_strings
        .into_iter()
        .map(|s| (s, s))
        .chain(other_test_strings);
    for (input, expected) in test_strings {
        let pat = Pattern::parse(input).map_err(|e| e.to_string()).unwrap();
        assert_eq!(pat.to_string(), expected);
    }

    for input in failures {
        assert!(
            Pattern::parse(input).is_err(),
            "Pattern parsed correctly but shouldn't: `{input}`"
        );
    }
}
