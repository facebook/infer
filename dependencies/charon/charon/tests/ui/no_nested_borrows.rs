pub struct Pair<T1, T2> {
    pub x: T1,
    pub y: T2,
}

pub enum List<T> {
    Cons(T, Box<List<T>>),
    Nil,
}

/// Sometimes, enumerations with one variant are not treated
/// the same way as the other variants (for example, downcasts
/// are not always introduced).
/// A downcast is the cast of an enum to a specific variant, like
/// in the left value of:
/// `((_0 as Right).0: T2) = move _1;`
pub enum One<T1> {
    One(T1),
}

/// Truely degenerate case
/// Instantiations of this are encoded as constant values by rust.
pub enum EmptyEnum {
    Empty,
}

/// Enumeration (several variants with no parameters)
/// Those are not encoded as constant values.
pub enum Enum {
    Variant1,
    Variant2,
}

/// Degenerate struct
/// Instanciations of this are encoded as constant values by rust.
pub struct EmptyStruct {}

pub enum Sum<T1, T2> {
    Left(T1),
    Right(T2),
}

pub struct Tuple<T1, T2>(T1, T2);

pub fn use_tuple_struct(x: &mut Tuple<u32, u32>) {
    x.0 = 1;
}

pub fn create_tuple_struct(x: u32, y: u64) -> Tuple<u32, u64> {
    Tuple(x, y)
}

pub fn create_pair(x: u32, y: u64) -> Pair<u32, u64> {
    Pair { x, y }
}

/// Structure with one field
pub struct IdType<T>(T);

pub fn use_id_type<T>(x: IdType<T>) -> T {
    x.0
}

pub fn create_id_type<T>(x: T) -> IdType<T> {
    IdType(x)
}

pub fn cast_u32_to_i32(x: u32) -> i32 {
    x as i32
}

pub fn cast_bool_to_i32(x: bool) -> i32 {
    x as i32
}

pub fn cast_bool_to_bool(x: bool) -> bool {
    x as bool
}

#[allow(unused_variables)]
pub fn test2() {
    let x: u32 = 23;
    let y: u32 = 44;
    let z = x + y;
    let p: Pair<u32, u32> = Pair { x, y: z };
    let s: Sum<u32, bool> = Sum::Right(true);
    let o: One<u64> = One::One(3);
    let e0 = EmptyEnum::Empty;
    let e1 = e0;
    let enum0 = Enum::Variant1;
}

pub fn get_max(x: u32, y: u32) -> u32 {
    if x >= y {
        x
    } else {
        y
    }
}

/// Box creation
#[allow(unused_variables)]
pub fn test_list1() {
    let l: List<i32> = List::Cons(0, Box::new(List::Nil));
}

/// Box deref
pub fn test_box1() {
    use std::ops::Deref;
    use std::ops::DerefMut;
    let mut b: Box<i32> = Box::new(0);
    let x = b.deref_mut();
    *x = 1;
    let x = b.deref();
    assert!(*x == 1);
}

pub fn copy_int(x: i32) -> i32 {
    x
}

/// Just checking the parameters given to unreachable
/// Rk.: the input parameter prevents using the function as a unit test.
pub fn test_unreachable(b: bool) {
    if b {
        unreachable!();
    }
}

/// Just checking the parameters given to panic
/// Rk.: the input parameter prevents using the function as a unit test.
// FIXME: broken because we don't support string constants
// pub fn test_panic(b: bool) {
//     if b {
//         panic!("Panicked!");
//     }
// }

pub fn is_cons<T>(l: &List<T>) -> bool {
    match l {
        List::Cons(_, _) => true,
        List::Nil => false,
    }
}

pub fn split_list<T>(l: List<T>) -> (T, List<T>) {
    match l {
        List::Cons(hd, tl) => (hd, *tl),
        _ => panic!(),
    }
}

/// Test with a char literal - testing serialization
pub fn test_char() -> char {
    'a'
}

/// Mutually recursive types
pub enum Tree<T> {
    Leaf(T),
    Node(T, NodeElem<T>, Box<Tree<T>>),
}

pub enum NodeElem<T> {
    Cons(Box<Tree<T>>, Box<NodeElem<T>>),
    Nil,
}

/// Mutually recursive functions
pub fn even(x: u32) -> bool {
    if x == 0 {
        true
    } else {
        odd(x - 1)
    }
}

pub fn odd(x: u32) -> bool {
    if x == 0 {
        false
    } else {
        even(x - 1)
    }
}

pub fn test_even_odd() {
    assert!(even(0));
    assert!(even(4));
    assert!(odd(1));
    assert!(odd(5));
}

/// Testing constants (some constants are hard to retrieve from MIR, because
/// they are compiled to very low values).
/// We resort to the following structure to make rustc generate constants...
pub struct StructWithTuple<T1, T2> {
    p: (T1, T2),
}

pub fn new_tuple1() -> StructWithTuple<u32, u32> {
    StructWithTuple { p: (1, 2) }
}

pub fn new_tuple2() -> StructWithTuple<i16, i16> {
    StructWithTuple { p: (1, 2) }
}

pub fn new_tuple3() -> StructWithTuple<u64, i64> {
    StructWithTuple { p: (1, 2) }
}

/// Similar to [StructWithTuple]
pub struct StructWithPair<T1, T2> {
    p: Pair<T1, T2>,
}

pub fn new_pair1() -> StructWithPair<u32, u32> {
    // This actually doesn't make rustc generate a constant...
    // I guess it only happens for tuples.
    StructWithPair {
        p: Pair { x: 1, y: 2 },
    }
}

pub fn incr(x: &mut u32) {
    *x += 1;
}

pub fn read_then_incr(x: &mut u32) -> u32 {
    let r = *x;
    *x += 1;
    r
}
