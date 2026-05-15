//! Regression test: this caused a panic once.
pub enum Thing {
    A(Box<()>),
}

fn foo(_: Thing) {}
