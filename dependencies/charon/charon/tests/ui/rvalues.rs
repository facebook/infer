#![feature(core_intrinsics)]
#![feature(liballoc_internals)]
#![allow(internal_features)]

use std::ptr::addr_of;

fn addr_of() {
    let mut x = 0u32;
    let _: *const u32 = &x; // implicit coercion
    let _: *const u32 = addr_of!(x);
    let _: *mut u32 = &raw mut x;
}

fn literal_casts() {
    let _ = 0u64 as u8;
    let _ = 0u8 as u64;
    let _ = 0u64 as f64;
    let _ = f64::MIN as u64;
    let _ = f32::MAX as f64;
}

fn ptr_casts() {
    let array_ptr: *const [u32; 64] = &[0; 64];
    let _ = array_ptr as *const u32;

    let x = 0;
    let x: *const u8 = &x;
    let _ = x as *mut u8 as *const u8;
    let _ = x as usize;

    let _ = 0usize as *const u8;

    fn foo() {}
    let _ = foo as *const u8;
}

fn fn_casts() {
    fn foo() {}
    let _ = foo as fn();

    unsafe fn bar() {}
    let _ = bar as unsafe fn();

    let closure = |_: u8| ();
    let _ = closure as fn(u8);
    let _ = closure as unsafe fn(u8);
}

fn boxes() {
    let _ = std::boxed::box_new(42);
}

// We force stealing to get the optimized_mir of the function. Optimizing the whole file would
// remove most of our (indeed useless) assignments.
static STEAL: [(); transmute([1, 0]) as usize] = [(); 1];
const fn transmute(x: [u32; 2]) -> u64 {
    // When optimized, this becomes a built-in cast. Otherwise this is just a call to `transmute`.
    unsafe { std::mem::transmute::<[u32; 2], u64>(x) }
}

// We force stealing to get the optimized_mir of the function.
static STEAL2: [(); nullary_ops::<u32>()] = [(); 13];
const fn nullary_ops<T>() -> usize {
    let size = size_of::<T>();
    let align = align_of::<T>();
    // This is `const (false)` in the MIR we get, but `true` in const evaluation.
    let ub = core::intrinsics::ub_checks();

    #[repr(C)]
    struct Struct<T> {
        a: u8,
        b: T,
    }
    let offset = std::mem::offset_of!(Struct<T>, b);

    size + align + (ub as usize) + offset
}
