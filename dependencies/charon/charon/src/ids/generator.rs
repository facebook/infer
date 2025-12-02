use index_vec::Idx;
use std::marker::PhantomData;

#[derive(Debug, Clone, Copy)]
pub struct Generator<I: Idx> {
    counter: usize,
    phantom: PhantomData<I>,
}

impl<I: Idx> Generator<I> {
    pub fn new() -> Self {
        Self::new_with_init_value(I::from_usize(0))
    }

    pub fn new_with_init_value(index: I) -> Self {
        Generator {
            counter: index.index(),
            phantom: PhantomData,
        }
    }

    /// Get a fresh id from this generator.
    pub fn fresh_id(&mut self) -> I {
        let index = self.next_id();
        self.advance(1);
        index
    }

    /// Get the next id that would be emitted by `fresh_id`.
    pub fn next_id(&self) -> I {
        I::from_usize(self.counter)
    }

    /// Move the generator forward by the given delta.
    pub fn advance(&mut self, by: usize) {
        // The release version of the code doesn't check for overflows.
        // As the max usize is very large, overflows are extremely
        // unlikely. Still, it is extremely important for our code that
        // no overflows happen on the index counters.
        self.counter = self.counter.checked_add(by).unwrap();
    }
}

// Manual impl to avoid the `I: Default` bound.
impl<I: Idx> Default for Generator<I> {
    fn default() -> Self {
        Self {
            counter: Default::default(),
            phantom: Default::default(),
        }
    }
}
