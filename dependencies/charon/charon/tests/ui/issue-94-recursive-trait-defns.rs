pub trait Trait1: Sized {
    type T: Trait2;
}

pub trait Trait2: Trait1 {}

pub trait T1<T: T2<Self>>: Sized {}
pub trait T2<T: T1<Self>>: Sized {}

pub trait T3 {
    type T: T5;
}

pub trait T4: T3 {}

pub trait T5 {
    type T: T4;
}

pub trait T6<T: T7>: Sized {
    fn f(x: u64) -> () {
        T::g(x)
    }
}

pub trait T7: T6<Self> {
    fn g(_x: u64) -> ();
}
