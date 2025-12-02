#![feature(register_tool)]
#![register_tool(charon)]
#![register_tool(aeneas)]

#[charon::rename("BoolTest")]
pub trait BoolTrait {
    // Required method
    #[charon::rename("getTest")]
    fn get_bool(&self) -> bool;

    // Provided method
    #[charon::rename("retTest")]
    fn ret_true(&self) -> bool {
        true
    }
}

#[charon::rename("BoolImpl")]
impl BoolTrait for bool {
    fn get_bool(&self) -> bool {
        *self
    }
}

#[charon::rename("BoolFn")]
pub fn test_bool_trait<T>(x: bool) -> bool {
    x.get_bool() && x.ret_true()
}

#[charon::rename("TypeTest")]
type Test = i32;

#[charon::rename("VariantsTest")]
enum SimpleEnum {
    #[charon::rename("Variant1")]
    FirstVariant,
    SecondVariant,
    ThirdVariant,
}

#[charon::rename("StructTest")]
struct Foo {
    #[charon::rename("FieldTest")]
    field1: u32,
}

#[charon::rename("Const_Test")]
const C: u32 = 100 + 10 + 1;

#[aeneas::rename("_TypeAeneas36")]
type Test2 = u32;
