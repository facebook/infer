pub struct S<'a, K> {
    x: &'a K,
}

impl<'a, K> S<'a, K> {
    pub fn f<F>()
    where
        F: FnMut(&u32),
    {
    }

    pub fn g<F>()
    where
        for<'b> F: FnMut(&'b u32),
    {
    }
}

pub struct T {}

impl T {
    pub fn f<F>()
    where
        F: FnMut(&u32),
    {
    }
}
