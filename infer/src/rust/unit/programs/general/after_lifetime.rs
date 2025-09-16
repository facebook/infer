fn main() {
    let ptr: *const i32; // ptr -> -
    unsafe {
        {
            let x = 50; // x -> a, a -> 50
            ptr = &x; //x -> a, ptr -> a, a -> 50
        }
        // x dropped: ptr -> a, a -/->
        let _z = *ptr; // Undefined behavior: use after free
    }
}