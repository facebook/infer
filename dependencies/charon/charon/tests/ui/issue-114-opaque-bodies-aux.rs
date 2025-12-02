//@ skip
#[inline(always)]
pub fn inline_always() -> u32 {
    42
}

#[inline]
pub fn inline_sometimes() -> u32 {
    42
}

#[inline(never)]
pub fn inline_never() -> u32 {
    42
}

// Generics always have MIR in the crate data.
#[inline(never)]
pub fn inline_generic<T>() -> u32 {
    42
}
