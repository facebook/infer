extern crate env_logger;

/// Initialize the logger.
pub fn initialize_logger() {
    {
        // Initialize the logger only once (useful when running the driver in tests).
        use std::sync::atomic::{AtomicBool, Ordering};
        static LOGGER_INITIALIZED: AtomicBool = AtomicBool::new(false);
        if LOGGER_INITIALIZED.swap(true, Ordering::SeqCst) {
            return;
        }
    }

    use std::io::IsTerminal;
    use tracing_subscriber::prelude::*;
    tracing_subscriber::registry()
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .with(
            tracing_tree::HierarchicalLayer::new(1)
                .with_ansi(std::io::stderr().is_terminal())
                .with_indent_lines(true)
                .with_bracketed_fields(true)
                .with_timer(tracing_tree::time::Uptime::default()),
        )
        .init();
}

/// This macro computes the name of the function in which it is called and the line number.
/// We adapted it from:
/// <https://stackoverflow.com/questions/38088067/equivalent-of-func-or-function-in-rust>
#[macro_export]
macro_rules! code_location {
    ($color:ident) => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);

        let path: Vec<_> = name.split("::").collect();
        // The path looks like `crate::module::function::f`.
        let mut name = path.iter().rev().skip(1).next().unwrap().to_string();

        let line = line!();
        let file = file!();
        let mut location = format!("{file}:{line}");

        use std::io::IsTerminal;
        if std::io::stderr().is_terminal() {
            use colored::Colorize;
            name = name.$color().to_string();
            location = location.dimmed().to_string();
        }
        format!("in {name} at {location}")
    }};
}

/// A custom log trace macro. Uses the log crate.
#[macro_export]
macro_rules! trace {
    ($($arg:tt)+) => {{
        tracing::trace!("{}:\n{}", $crate::code_location!(yellow), format!($($arg)+))
    }};
    () => {{
        tracing::trace!("{}", $crate::code_location!(yellow))
    }};
}

/// A custom log error macro. Uses the log crate.
#[macro_export]
macro_rules! error {
    ($($arg:tt)+) => {{
        tracing::error!("{}:\n{}", $crate::code_location!(red), format!($($arg)+))
    }};
}

/// A custom log warn macro. Uses the log crate.
#[macro_export]
macro_rules! warn {
    ($($arg:tt)+) => {{
        tracing::warn!("{}:\n{}", $crate::code_location!(yellow), format!($($arg)+))
    }};
}

/// A custom log info macro. Uses the log crate.
#[macro_export]
macro_rules! info {
    ($($arg:tt)+) => {{
        // As for info we generally output simple messages, we don't insert a newline.
        tracing::info!("{}: {}", $crate::code_location!(yellow), format!($($arg)+))
    }};
    () => {{
        tracing::info!("{}", $crate::code_location!(yellow))
    }};
}
