pub trait BoolTrait {
    // Required method
    fn get_bool(&self) -> bool;

    // Provided method
    fn ret_true(&self) -> bool {
        true
    }
}

impl BoolTrait for bool {
    fn get_bool(&self) -> bool {
        *self
    }
}

pub fn test_bool_trait_bool(x: bool) -> bool {
    x.get_bool() && x.ret_true()
}

impl<T> BoolTrait for Option<T> {
    fn get_bool(&self) -> bool {
        match self {
            Option::Some(_) => true,
            Option::None => false,
        }
    }
}

pub fn test_bool_trait_option<T>(x: Option<T>) -> bool {
    x.get_bool() && x.ret_true()
}

pub fn test_bool_trait<T: BoolTrait>(x: T) -> bool {
    x.get_bool()
}

pub trait ToU64 {
    fn to_u64(self) -> u64;
}

impl ToU64 for u64 {
    fn to_u64(self) -> u64 {
        self
    }
}

impl<A: ToU64> ToU64 for (A, A) {
    fn to_u64(self) -> u64 {
        self.0.to_u64() + self.1.to_u64()
    }
}

pub fn f<T: ToU64>(x: (T, T)) -> u64 {
    x.to_u64()
}

pub fn g<T>(x: (T, T)) -> u64
where
    (T, T): ToU64,
{
    x.to_u64()
}

pub fn h0(x: u64) -> u64 {
    x.to_u64()
}

pub struct Wrapper<T> {
    x: T,
}

impl<T: ToU64> ToU64 for Wrapper<T> {
    fn to_u64(self) -> u64 {
        self.x.to_u64()
    }
}

pub fn h1(x: Wrapper<u64>) -> u64 {
    x.to_u64()
}

pub fn h2<T: ToU64>(x: Wrapper<T>) -> u64 {
    x.to_u64()
}

pub trait ToType<T> {
    fn to_type(self) -> T;
}

impl ToType<bool> for u64 {
    fn to_type(self) -> bool {
        self > 0
    }
}

pub trait OfType {
    fn of_type<T: ToType<Self>>(x: T) -> Self
    where
        Self: std::marker::Sized;
}

pub fn h3<T1: OfType, T2: ToType<T1>>(y: T2) -> T1 {
    T1::of_type(y)
}

// Checking what happens if we move trait clauses from a method to its enclosing block
pub trait OfTypeBis<T: ToType<Self>>
where
    Self: std::marker::Sized,
{
    fn of_type(x: T) -> Self
    where
        Self: std::marker::Sized;
}

pub fn h4<T1: OfTypeBis<T2>, T2: ToType<T1>>(y: T2) -> T1 {
    T1::of_type(y)
}

pub struct TestType<T>(T);

// Checking what happens with nested blocks
impl<T: ToU64> TestType<T> {
    pub fn test(&self, x: T) -> bool {
        struct TestType1(u64);
        trait TestTrait {
            fn test(&self) -> bool;
        }

        // Remark: we can't write: impl TestTrait for TestType<T>,
        // we have to use a *local* parameter (can't use the outer T).
        // In other words: the parameters used in the items inside
        // an impl must be bound by the impl block (can't come from outer
        // blocks).

        impl TestTrait for TestType1 {
            fn test(&self) -> bool {
                self.0 > 1
            }
        }

        let x = x.to_u64();
        let y = TestType1(0);
        x > 0 && y.test()
    }
}

pub struct BoolWrapper(pub bool);

impl<T> ToType<T> for BoolWrapper
where
    bool: ToType<T>,
{
    fn to_type(self) -> T {
        self.0.to_type()
    }
}

pub trait WithConstTy<const LEN: usize> {
    const LEN1: usize;
    // Testing default values
    const LEN2: usize = 32;

    type V;
    type W: ToU64;

    // Below: we can't use [Self::Len1] in the type of the array.
    // Probably because of dyn traits...
    fn f(x: &mut Self::W, y: &[u8; LEN]);
}

impl WithConstTy<32> for bool {
    const LEN1: usize = 12;

    type V = u8;
    type W = u64;

    fn f(_: &mut Self::W, _: &[u8; 32]) {}
}

pub fn use_with_const_ty1<const LEN: usize, H: WithConstTy<LEN>>() -> usize {
    H::LEN1
}

pub fn use_with_const_ty2<const LEN: usize, H: WithConstTy<LEN>>(_: H::W) {}

pub fn use_with_const_ty3<const LEN: usize, H: WithConstTy<LEN>>(x: H::W) -> u64 {
    x.to_u64()
}

pub fn test_where1<'a, T: 'a>(_x: &'a T) {}
pub fn test_where2<T: WithConstTy<32, V = u32>>(_x: T::V) {}

// Below: testing super traits.
//
// Actually, this comes for free: ChildTrait : ParentTrait just adds a trait
// clause for Self: `Self : ParentTrait`.
pub trait ParentTrait0 {
    type W;
    fn get_name(&self) -> String;
    fn get_w(&self) -> Self::W;
}
pub trait ParentTrait1 {}
pub trait ChildTrait: ParentTrait0 + ParentTrait1 {}

// But we still need to correctly reference the traits
pub fn test_child_trait1<T: ChildTrait>(x: &T) -> String {
    x.get_name()
}

pub fn test_child_trait2<T: ChildTrait>(x: &T) -> T::W {
    x.get_w()
}

// Checking if the order has an importance (we use U::W before we declare that
// U:ParentTrait0)
pub fn order1<T: ParentTrait0<W = U::W>, U: ParentTrait0>() {}

/* */
pub trait ChildTrait1: ParentTrait1 {}

impl ParentTrait1 for usize {}
impl ChildTrait1 for usize {}

/* [IntoIterator] is interesting because of the condition [Item = Self::Item]
for the [IntoIter] associated type. */
pub trait Iterator {
    type Item;
}

pub trait IntoIterator {
    type Item;
    type IntoIter: Iterator<Item = Self::Item>;

    // Required method
    fn into_iter(self) -> Self::IntoIter;
}

/* The traits below are inspired by [Try] and [FromResidual].

   The reference to `Self as Try` in the `FromResidual` clause used to
   cause a bug.
*/
trait Try: FromResidual<<Self as Try>::Residual> {
    type Residual;
}

trait FromResidual<T> {}

pub trait WithTarget {
    type Target;
}

pub trait ParentTrait2 {
    type U: WithTarget;
}

pub trait ChildTrait2: ParentTrait2 {
    fn convert(x: Self::U) -> <Self::U as WithTarget>::Target;
}

impl WithTarget for u32 {
    type Target = u32;
}

impl ParentTrait2 for u32 {
    type U = u32;
}

impl ChildTrait2 for u32 {
    fn convert(x: u32) -> u32 {
        x
    }
}

/*
// This one requires a lot of traits
pub fn test_enumerate(x: usize) {
    for _ in 0..x {}
}
*/

/* Custom function pointers */
pub trait CFnOnce<Args> {
    type Output;

    fn call_once(self, args: Args) -> Self::Output;
}

pub trait CFnMut<Args>: CFnOnce<Args> {
    fn call_mut(&mut self, args: Args) -> Self::Output;
}

pub trait CFn<Args>: CFnMut<Args> {
    fn call(&self, args: Args) -> Self::Output;
}

pub trait GetTrait {
    type W;
    fn get_w(&self) -> Self::W;
}

pub fn test_get_trait<T: GetTrait>(x: &T) -> T::W {
    x.get_w()
}

// Constants with generics
pub trait Trait {
    const LEN: usize;
}

impl<const N: usize, T> Trait for [T; N] {
    const LEN: usize = N;
}

impl<T: Trait> Trait for Wrapper<T> {
    const LEN: usize = 0;
}

pub fn use_wrapper_len<T: Trait>() -> usize {
    Wrapper::<T>::LEN
}

pub struct Foo<T, U> {
    pub x: T,
    pub y: U,
}

impl<T: Trait, U> Foo<T, U> {
    pub const FOO: Result<T, i32> = Err(0);
}

pub fn use_foo1<T: Trait, U>() -> Result<T, i32> {
    Foo::<T, U>::FOO
}

pub fn use_foo2<T, U: Trait>() -> Result<U, i32> {
    Foo::<U, T>::FOO
}

// Recursive trait impl
trait RecursiveImpl {
    type Item: RecursiveImpl;
}
impl RecursiveImpl for () {
    type Item = ();
}

pub fn flabada<'a>(_x: &'a ()) -> Wrapper<(bool, &'a ())> {
    todo!()
}

pub fn call<'a, F: Fn(&'a ()) -> Wrapper<(bool, &'a ())>>(_: F) {}

pub fn flibidi() -> () {
    call(flabada);
}

mod assoc_ty_trait_ref {
    //! Minimal reproducer for a binder bug
    trait SliceIndex {
        type Output;
    }
    fn index<I: SliceIndex>() -> I::Output {
        todo!()
    }
}
