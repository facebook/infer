pub fn fun1() {
    crate::opaque::fun2()
}

pub fn fun3() {
    let _ = "".contains("");
    let _ = crate::opaque::takes_pattern::<&str>();
}
