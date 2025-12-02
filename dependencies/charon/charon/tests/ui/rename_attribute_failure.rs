//@ known-failure
#![feature(register_tool)]
#![register_tool(charon)]
#![register_tool(aeneas)]

#[charon::rename("")]
pub type TestEmpty = ();

#[charon::rename("Test!976?")]
pub type TestNonAlphanumeric = ();

#[charon::rename("75Test")]
pub type TestNotStartingWithLetter = ();

#[charon::rename(aaaa)]
pub type TestNotQuoted = ();

#[charon::rename("_Type36")]
#[aeneas::rename("_Type37")]
pub type TestMultiple = ();

#[charon::something_else("_Type36")]
pub type TestUnrecognizedArg = ();
