struct Foo<'a, T>(&'a T);

impl<'a, T> Foo<'a, T>
where
    T: Clone,
{
    pub fn test_nested_closures<'b>(x: &'a &'b T) -> T {
        let clo = || |_y: &u32| |_z: &u32| (*x).clone();
        clo()(&0)(&1)
    }
}
