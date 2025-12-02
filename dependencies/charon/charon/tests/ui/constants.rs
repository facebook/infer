//! Tests with constants

// Integers

pub const X0: u32 = 0;

pub const X1: u32 = u32::MAX;

#[allow(clippy::let_and_return)]
pub const X2: u32 = {
    let x = 3;
    x
};

pub const X3: u32 = incr(32);

pub const fn incr(n: u32) -> u32 {
    n + 1
}

// Pairs

pub const fn mk_pair0(x: u32, y: u32) -> (u32, u32) {
    (x, y)
}

pub const fn mk_pair1(x: u32, y: u32) -> Pair<u32, u32> {
    Pair { x, y }
}

pub const P0: (u32, u32) = mk_pair0(0, 1);
pub const P1: Pair<u32, u32> = mk_pair1(0, 1);
pub const P2: (u32, u32) = (0, 1);
pub const P3: Pair<u32, u32> = Pair { x: 0, y: 1 };

pub struct Pair<T1, T2> {
    pub x: T1,
    pub y: T2,
}

pub const Y: Wrap<i32> = Wrap::new(2);

pub const fn unwrap_y() -> i32 {
    Y.value
}

pub const YVAL: i32 = unwrap_y();

pub struct Wrap<T> {
    value: T,
}

impl<T> Wrap<T> {
    pub const fn new(value: T) -> Wrap<T> {
        Wrap { value }
    }
}

// Additions

pub const fn get_z1() -> i32 {
    const Z1: i32 = 3;
    Z1
}

pub const fn add(a: i32, b: i32) -> i32 {
    a + b
}

pub const fn get_z2() -> i32 {
    add(Q1, add(get_z1(), Q3))
}

pub const Q1: i32 = 5;
pub const Q2: i32 = Q1;
pub const Q3: i32 = add(Q2, 3);

// Static

pub static S1: u32 = 6;
pub static S2: u32 = incr(S1);
pub static S3: Pair<u32, u32> = P3;
pub static S4: Pair<u32, u32> = mk_pair1(7, 8);

// Constants with generics
pub struct V<const N: usize, T> {
    pub x: [T; N],
}

impl<const N: usize, T> V<N, T> {
    pub const LEN: usize = N;
}

pub fn use_v<const N: usize, T>() -> usize {
    V::<N, T>::LEN
}
