struct V<const N: usize, T> {
    x: [T; N],
}

impl<const N: usize, T> V<N, T> {
    const LEN: usize = N; // This has generics <N, T>
}

trait HasLen {
    const LEN: usize;
}

impl<const N: usize> HasLen for [(); N] {
    const LEN: usize = N;
}

impl<const N: usize> HasLen for [bool; N] {
    const LEN: usize = N + 1;
}

/// The assoc const depends on a generic parameter.
struct Wrapper<T>(T);
impl<T: HasLen> HasLen for Wrapper<T> {
    const LEN: usize = T::LEN;
}

pub trait HasDefaultLen<const M: usize> {
    const LEN: usize = M;
    // fn use_array(_a: [(); Self::LEN]) {}
}

impl<const N: usize> HasDefaultLen<N> for [(); N] {}

impl<const N: usize> HasDefaultLen<N> for [bool; N] {
    // Recursive constant because we can
    const LEN: usize = if true {
        N
    } else {
        <Self as HasDefaultLen<N>>::LEN
    };
}

/// The default assoc const depends the `Self` clause: we must be careful when reusing this default
/// value in impls.
trait AlsoHasLen: HasLen {
    const ALSO_LEN: usize = Self::LEN + 1;
}
impl<const N: usize> AlsoHasLen for [(); N] {}
