trait Ord {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering;
}

fn min<T: Ord>() {
    let _ = T::cmp;
}
