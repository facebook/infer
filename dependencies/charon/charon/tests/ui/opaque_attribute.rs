#![feature(register_tool)]
#![register_tool(charon)]
#![register_tool(aeneas)]

#[charon::opaque]
pub trait BoolTrait {
    // Required method
    #[charon::opaque]
    fn get_bool(&self) -> bool;

    // Provided method
    #[charon::opaque]
    fn ret_true(&self) -> bool {
        true
    }
}

#[charon::opaque]
impl BoolTrait for bool {
    fn get_bool(&self) -> bool {
        *self
    }
}

#[charon::opaque]
impl<T: BoolTrait> BoolTrait for Option<T> {
    fn get_bool(&self) -> bool {
        match self {
            Some(x) => x.get_bool(),
            None => false,
        }
    }
}

#[charon::opaque]
pub fn test_bool_trait_option<T: BoolTrait>(x: Option<T>) -> bool {
    x.get_bool() && x.ret_true()
}

#[charon::opaque]
type Test = i32;

#[charon::opaque]
const SIX_SIX_SIX: u32 = 600 + 60 + 6;

#[aeneas::opaque]
type Test2 = u32;

fn call_fn_in_opaque_module() {
    let _ = opaque::fn_in_opaque_module();
}

#[charon::opaque]
mod opaque {
    // This one will be translated because it is called.
    pub fn fn_in_opaque_module() -> u32 {
        42
    }
    // This one will be skipped because the module is opaque hence not traversed.
    pub fn other_fn_in_opaque_module() -> u32 {
        42
    }
}
