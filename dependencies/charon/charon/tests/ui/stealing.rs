// Translating this needs the evaluatable MIR of `fn four()`, which steals its `mir_built` body.
// There's no simple ordering of the translation of items that can avoid all stealing.
static SLICE: [(); four()] = [(); 4];

const fn four() -> usize {
    2 + 2
}

// The order counts, we want to translate `BAR` first to steal `FOO`.
const BAR: [(); FOO] = [(); FOO];
const FOO: usize = 42;
