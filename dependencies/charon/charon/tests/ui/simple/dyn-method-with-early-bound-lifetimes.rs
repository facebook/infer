trait Trait {
    fn foo<'a>(&self)
    where
        Self: 'a;
}

fn foo(x: &dyn Trait) {}
