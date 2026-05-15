struct A<'a, 'b> {
    x: &'a mut u32,
    y: &'b u32,
    z: Box<&'b mut u32>,
}

struct B<'a, 'b>(A<'a, 'b>);
