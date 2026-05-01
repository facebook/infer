trait Convert<T>: Sized {
    fn method(self) -> T;
}

impl<'a> Convert<&'a [u8; 4]> for &'a u32 {
    fn method(self) -> &'a [u8; 4] {
        unsafe { &*(self as *const u32).cast::<[u8; 4]>() }
    }
}

fn convert<T, U>(x: T) -> U
where
    T: Convert<U>,
{
    x.method()
}

pub fn to_bytes_by_ref<'b>(s: &'b u32) -> &'b [u8; 4] {
    // Here `convert` is given generics `<&'_ u32, &'_ [u8; 4]>`, which loses the information
    // that the lifetime is the same by virtue of the trait impl.
    convert(s)
}
