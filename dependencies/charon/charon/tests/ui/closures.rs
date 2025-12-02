//@ charon-args=--remove-associated-types=*
pub fn incr_u32(x: u32) -> u32 {
    x + 1
}

/* Testing function pointers and closures */
// TODO: this requires to take into account the constraints over associated types
// because the output type of the Fn trait is an associated type, not a parameter.
// More precisely, we have the constraint that:
// <F as Fn<T>>::Output = T
#[allow(clippy::manual_map)]
pub fn map_option<T, F>(x: Option<T>, f: F) -> Option<T>
where
    F: Fn(T) -> T,
{
    match x {
        None => None,
        Some(x) => Some(f(x)),
    }
}

#[allow(clippy::manual_map, clippy::needless_lifetimes)]
pub fn map_option_pointer_ref<'a, T, F>(x: &'a Option<T>, f: fn(&T) -> T) -> Option<T> {
    match x {
        None => None,
        Some(x) => Some(f(x)),
    }
}

// With a pointer to a top level function
pub fn test_map_option1(x: Option<u32>) -> Option<u32> {
    map_option(x, incr_u32)
}

// Local function
pub fn test_closure_u32(x: u32) -> u32 {
    let f: fn(u32) -> u32 = |x| x;
    (f)(x)
}

pub fn test_closure_u32s(x: u32) -> u32 {
    let f: fn(u32, u32) -> u32 = |x, y| x + y;
    (f)(x, x)
}

// Local function with reference
//
// The lifetime 'a could be omitted, but we insert it to check that the
// lifetimes are correctly retrieved by Charon (in particular, that it doesn't
// mix the lifetimes used in the closure with the lifetimes used by the top-level
// function).
#[allow(clippy::needless_lifetimes)]
pub fn test_closure_ref_u32<'a>(x: &'a u32) -> &'a u32 {
    let f: fn(&u32) -> &u32 = |y| y;
    (f)(x)
}

// Local function which uses local type variables
pub fn test_closure_ref_param<T>(x: &T) -> &T {
    let f: fn(&T) -> &T = |x| x;
    (f)(x)
}

pub trait Trait<'a> {}

pub fn test_closure_ref_early_bound<'a, T: Trait<'a>>(x: &'a T) -> &'a T {
    let f: fn(&T) -> &T = |y| y;
    (f)(x)
}

// TODO: what happens if we use a top-level function with generic parameters
// and where clauses? Where does the instantiation and the trait resolutionb
// happen?

// TODO: Local function which uses local lifetime variables

// With a local function
pub fn test_map_option2(x: Option<u32>) -> Option<u32> {
    let f: fn(u32) -> u32 = |x| x + 1;
    map_option(x, f)
}

pub fn id<T>(x: T) -> T {
    x
}

pub fn test_map_option_id1(x: Option<u32>) -> Option<u32> {
    map_option(x, id)
}

pub fn test_map_option_id2(x: Option<u32>) -> Option<u32> {
    let f = id;
    map_option(x, f)
}

#[allow(clippy::redundant_clone)]
pub fn id_clone<T: Clone>(x: T) -> T {
    x.clone()
}

// Testing the trait resolution on function pointers
pub fn test_id_clone(x: u32) -> u32 {
    let f: fn(u32) -> u32 = id_clone;
    (f)(x)
}

// Testing the trait resolution on function pointers
pub fn test_id_clone_param<T: Clone>(x: T) -> T {
    let f: fn(T) -> T = id_clone;
    (f)(x)
}

// Testing the trait resolution on function pointers
pub fn test_map_option_id_clone(x: Option<u32>) -> Option<u32> {
    map_option(x, id_clone)
}

pub fn test_map_option3(x: Option<u32>) -> Option<u32> {
    map_option(x, |x| x + 1)
}

// With a closure which uses regions from the parent function.
// In the signature of the closure, the regions from the parent function
// are erased:
// ```text
// fn closures::test_regions::closure<'_0>(@1: (&'_0 (&'_ (u32)))) -> u32 { ... }
//                                                     ^^
// ```
pub fn test_regions<'a>(x: &'a u32) -> u32 {
    let f = |x: &&'a u32| **x;
    (f)(&x)
}

pub fn test_regions_casted<'a>(x: &'a u32) -> u32 {
    let f: fn(&&'a u32) -> u32 = |x| **x;
    (f)(&x)
}

pub fn test_closure_capture(x: u32, y: u32) -> u32 {
    let f = &|z| x + y + z;
    (f)(0)
}

pub fn test_closure_clone<T: Clone>(x: T) -> T {
    #[allow(clippy::redundant_clone)]
    let f = |x: T| x.clone();
    (&f)(x)
}

/*// With a `dyn`
pub fn test_map_option3(x: Option<u32>) -> Option<u32> {
    let f: fn(u32) -> u32 = |x| x + 1;
    map_option(x, f)
}*/
pub fn test_array_map(x: [i32; 256]) -> [i32; 256] {
    x.map(|v| v)
}

fn test_fnmut_with_ref() {
    let mut sum = 0_usize;
    let mut closure = |x: &usize| sum += *x;
    closure(&15);
}
