pub fn fun1() {
    crate::opaque::fun2()
}

#[charon::error]
pub fn fun3() {
    let x = error_crate::CausesError;
    let _y = error_crate::CausesError;
    let _ = crate::opaque::erroring_function(x);
}
