//@ charon-args=--remove-associated-types=*
trait DynableTrait {
    type DynableTraitType;
}

trait Trait {
    type TraitType: DynableTrait + ?Sized;
}

impl Trait for () {
    type TraitType = dyn DynableTrait<DynableTraitType = ()>;
}
