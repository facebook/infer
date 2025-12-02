pub trait Trait1 {
    type T: Trait2;
}

pub trait Trait2: Trait1 {
    type U;
}
