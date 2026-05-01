const CONST: u8 = 0;

fn f0() {
    if 0 < 1 {
        if 0 < 1 {}
        let x = CONST;
    }
}

fn f1( serialized: &[u8; 1]) {
    let previous_true_hints_seen = 0usize;

    let mut i = 0;

    while i < 1 {
        if (0 < 1) || (1 > 1) {}

        let mut j = 0;
        while j < 1 {
            let x = CONST;
        }
    }
}
