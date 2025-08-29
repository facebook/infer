pub fn swi_nested_complex(a: i32, b: i32, c: i32) -> i32 {
    if a > 0 {
        if b < 0 {
            if c == 0 { 1 } else { 2 }
        } else {
            if c > 10 { 3 } else { 4 }
        }
    } else if a == 0 {
        if b == 0 { 5 } else { 6 }
    } else {
        if b > 0 { 7 } else { 8 }
    }
}

fn main() {
    let x: i32 = 5;
    let y: i32 = -3;
    let z: i32 = 15;
    let _ = swi_nested_complex(x, y, z);
    
    // Test different branches
    let _ = swi_nested_complex(0, 0, 0);  // a == 0, b == 0
    let _ = swi_nested_complex(-1, 1, 0); // a < 0, b > 0
    let _ = swi_nested_complex(1, 1, 5);  // a > 0, b >= 0, c <= 10
}