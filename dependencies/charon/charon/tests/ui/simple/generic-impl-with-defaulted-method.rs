pub trait BoolTrait {
    fn foo(&self) {}
}

impl<T> BoolTrait for Option<T> {}
