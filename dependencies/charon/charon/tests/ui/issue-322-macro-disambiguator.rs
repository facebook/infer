macro_rules! transmute {
    () => {{
        struct AssertIsAsBytes;
        let _ = AssertIsAsBytes;
    }};
}

fn main() {
    transmute!();
    transmute!();
}
