/// Test with a static lifetime
fn test_static(x: &'static u32) -> &'static u32 {
    x
}

struct SStatic {
    x: &'static u32,
}

/// Type with simple regions hierarchy
enum E0<'a, 'b, T1, T2> {
    V1(&'a mut T1, &'b mut T2),
}

/// Types with complex regions hierarchy
enum E1<'a, 'b, T1, T2> {
    V1(&'a mut T1, &'b mut T2),
    /// Invert the type parameters, but not the region parameters
    V2(Box<E1<'a, 'b, T2, T1>>),
}

enum E2<'a, 'b, T1, T2> {
    V1(&'a mut T1, &'b mut T2),
    /// Invert the region parameters, but not the type parameters
    V3(Box<E2<'b, 'a, T1, T2>>),
}

enum E3<'a, 'b, 'c, T1, T2> {
    V1(&'a mut T1, &'b mut T2),
    /// Invert the type parameters, but not the region parameters
    V2(Box<E3<'a, 'b, 'c, T2, T1>>),
    /// Invert the region parameters, but not the type parameters
    V3(Box<E3<'b, 'a, 'c, T1, T2>>),
    V4(&'c &'a T1),
}

enum E4<'a, 'b, 'c, T1, T2, T3> {
    V1(&'a mut T1, &'b mut T2),
    /// Invert the type parameters, but not the region parameters
    V2(Box<E4<'a, 'b, 'c, T2, T1, T3>>),
    /// Invert the region parameters, but not the type parameters
    V3(Box<E4<'b, 'a, 'c, T1, T2, T3>>),
    V4(&'c &'a T3),
}

enum E5<'a, 'b, 'c, T1, T2, T3> {
    V1(&'a mut T1, &'b mut T2),
    /// Invert the type parameters, but not the region parameters
    V2(Box<E5<'a, 'b, 'c, T2, T1, T3>>),
    /// Invert the region parameters, but not the type parameters
    V3(Box<E5<'b, 'a, 'c, T1, T2, T3>>),
    V4(&'a &'c T3),
}

struct S1<'a, 'b, 'c, 'd> {
    x: E1<'a, 'b, &'c mut u32, &'d u32>,
}
