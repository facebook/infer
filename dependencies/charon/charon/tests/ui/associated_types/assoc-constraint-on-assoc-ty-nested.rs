//@ charon-args=--remove-associated-types=*
trait Trait {
    type Assoc;
}

trait IntoIterator {
    type IntoIter: Trait<Assoc = ()>;
}

trait IntoIntoIterator {
    type IntoIntoIter: IntoIterator;
}

// We should be able to deduce that `Clause2_Assoc = ()`, but this requires unification of
// identical trait clauses.
fn foo<I>()
where
    I: IntoIntoIterator,
    <<I as IntoIntoIterator>::IntoIntoIter as IntoIterator>::IntoIter: Trait,
{
}
