#![feature(register_tool)]
#![register_tool(charon)]

#[charon::error]
pub struct CausesError;
