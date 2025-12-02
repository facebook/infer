fn function_call(_: u32) {}

// Comment0
pub fn sum(s: &[u32]) -> u32 {
    // `let sum`
    let mut sum = 0;
    // `let i`
    //  indented sub-comment
    //unindented sub-comment
    let mut i = 0;
    // Function call
    function_call(sum + 2);
    // Start of loop
    while i < s.len() {
        // Add to running sum
        sum += s[i];
        // Increment `i`
        i += 1;
        // Before end of loop
    }
    // Assign the result of an `if`.
    sum = if sum > 10 {
        // sum + 100
        sum + 100
    } else {
        // let sum untouched
        sum
    };
    // Function call
    function_call(sum + 2);
    // Return final value
    sum
}

#[derive(Default)]
struct Foo {
    x: u32,
    y: u32,
}

#[derive(Default)]
struct Bar {
    x: u32,
    super_long_field_name: u32,
}

fn eat<T>(_: T) {}

fn foo() {
    // Call `default` and destructure the result
    let Foo { x, y } = Default::default();
    // Call `eat` on an aggregate value
    eat(Foo { x, y });

    // Call `default` and destructure the result
    let Bar {
        x,
        // This is the long field
        super_long_field_name,
    } = Default::default();
    // Call `eat` on an aggregate value
    eat(Bar {
        x,
        // This is the long field
        super_long_field_name,
    });

    // Build an array
    let a = [0u32; 10];
    // `assert_eq`
    assert_eq!(a[9], 9);
}

const CONSTANT: u32 = 42;

pub fn thing() {
    // This comment belongs above the assignment to `x` and not above intermediate computations.
    let x = (CONSTANT >> 3) + 12;
    function_call(x);
}

pub fn fake_read(x: u32) {
    // This statement is translated to a `fake_read`.
    let _ = x;
}

// The heuristic is easy to fool
fn fool() {
    let _ = "
    // Fooled ya";
    ()
}
