//@ charon-args=--lift-associated-types=_
pub trait WithTarget {
    type Target;
}

pub trait ParentTrait {
    type FromParent: WithTarget;
}

pub trait ChildTrait: ParentTrait {
    fn convert(x: Self::FromParent) -> <Self::FromParent as WithTarget>::Target;
}

impl WithTarget for u32 {
    type Target = u32;
}

impl ParentTrait for u32 {
    type FromParent = u32;
}

impl ChildTrait for u32 {
    fn convert(x: u32) -> u32 {
        x
    }
}
