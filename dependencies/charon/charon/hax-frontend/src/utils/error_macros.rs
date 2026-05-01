macro_rules! format_with_context {
    ($format_str:expr $(,$arg:expr)* $(; {$($x:expr),*})?) => {
        format!(
            concat!(
                $format_str
                $(, "\n\nContext:\n", $(concat!(" - ", stringify!($x), ": "), "{:#?}", "\n",)*)?
            ),
            $($arg,)*
            $($($x,)*)?
        )
    };
    ($($tt:tt)*) => {format!($($tt)*)};
}

mod internal_helpers {
    macro_rules! _verb {
        (fatal, $o:expr, $message:expr) => {
            $o.struct_fatal($message)
        };
        (error, $o:expr, $message:expr) => {
            $o.struct_err($message)
        };
        (warn, $o:expr, $message:expr) => {
            $o.struct_warn($message)
        };
    }
    macro_rules! _span_verb_base {
        ($verb:ident, $s:ident, $span:expr, $message:expr) => {{
            let backtrace = std::backtrace::Backtrace::capture();
            eprintln!("{}", backtrace);
            let mut builder = $crate::utils::_verb!($verb, $s.base().tcx.dcx(), $message);
            if let Some(span) = $span {
                builder.span(span.clone());
            }
            builder.code(rustc_errors::codes::ErrCode::MAX);
            builder.emit()
        }};
    }

    pub(crate) use _span_verb_base;
    pub(crate) use _verb;
}

macro_rules! report {
    ($verb:ident, $s:ident [$span:expr], $($tt:tt)*) => {
        $crate::utils::_span_verb_base!($verb, $s, Some($span), $crate::utils::format_with_context!($($tt)*))
    };
    ($verb:ident, $s:ident, $($tt:tt)*) => {
        $crate::utils::_span_verb_base!(
            $verb,
            $s,
            $s.base().opt_def_id.map(|did| $s.base().tcx.def_span(did)),
            $crate::utils::format_with_context!($($tt)*)
        )
    };
}

#[allow(unused_macros)]
macro_rules! warning { ($($tt:tt)*) => {$crate::utils::report!(warn, $($tt)*)} }
macro_rules! fatal { ($($tt:tt)*) => {$crate::utils::report!(fatal, $($tt)*)} }

pub(crate) use format_with_context;
pub(crate) use internal_helpers::_span_verb_base;
pub(crate) use internal_helpers::_verb;
pub(crate) use report;

macro_rules! supposely_unreachable_message {
    ($label:literal) => {
        concat!(
            "Supposely unreachable place in the Rust AST. The label is ",
            stringify!($label),
            ".\nThis error report happend because some assumption about the Rust AST was broken."
        )
    };
}

macro_rules! supposely_unreachable_fatal {
    ($s:ident $([$span:expr])?, $label:literal $($tt:tt)*) => {
        $crate::utils::fatal!($s$([$span])?, $crate::utils::supposely_unreachable_message!($label) $($tt)+)
    };
}

pub(crate) use fatal;
pub(crate) use supposely_unreachable_fatal;
pub(crate) use supposely_unreachable_message;
#[allow(unused_imports)]
pub(crate) use warning;

pub trait SExpect: Sized {
    type Output;
    fn s_expect<'tcx, S: crate::BaseState<'tcx>>(self, s: &S, message: &str) -> Self::Output;

    fn s_unwrap<'tcx, S: crate::BaseState<'tcx>>(self, s: &S) -> Self::Output {
        self.s_expect(s, "")
    }
}

mod s_expect_impls {
    use super::*;
    struct Dummy;
    impl std::fmt::Debug for Dummy {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "...")
        }
    }

    fn s_expect_error<'tcx>(
        s: &impl crate::BaseState<'tcx>,
        expected: impl std::fmt::Debug,
        got: impl std::fmt::Debug,
        message: &str,
    ) -> ! {
        fatal!(
            s,
            "s_expect: expected {:?}, got {:?}. {}",
            expected,
            got,
            message
        )
    }

    impl<T: std::fmt::Debug> SExpect for Option<T> {
        type Output = T;
        fn s_expect<'tcx, S: crate::BaseState<'tcx>>(self, s: &S, message: &str) -> Self::Output {
            self.unwrap_or_else(|| s_expect_error(s, Some(Dummy), None::<()>, message))
        }
    }

    impl<T: std::fmt::Debug, E: std::fmt::Debug> SExpect for Result<T, E> {
        type Output = T;
        fn s_expect<'tcx, S: crate::BaseState<'tcx>>(self, s: &S, message: &str) -> Self::Output {
            self.unwrap_or_else(|e| s_expect_error(s, Ok::<_, ()>(Dummy), Err::<(), _>(e), message))
        }
    }
}
