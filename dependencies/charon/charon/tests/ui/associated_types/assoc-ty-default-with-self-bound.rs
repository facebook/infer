#![feature(associated_type_defaults)]
pub trait MyTrait {
    type Ret: Sized
        = ()
    where
        Self: Sized;
}

pub struct S;
impl MyTrait for S {}
