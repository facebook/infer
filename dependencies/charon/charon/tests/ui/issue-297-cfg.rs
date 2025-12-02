fn f1(a: &[u8]) -> usize {
    let mut sampled = 0;
    if a[0] < 42 && a[1] < 16 {
        sampled += 100;
    }
    sampled += 101;
    sampled
}

const FIELD_MODULUS: i16 = 8;
fn f2(a: &[u8], result: &mut [i16]) -> usize {
    let mut sampled = 0;
    for bytes in a.chunks(3) {
        let b1 = bytes[0] as i16;
        let b2 = bytes[1] as i16;
        let b3 = bytes[2] as i16;

        let d1 = ((b2 & 0xF) << 8) | b1;
        let d2 = (b3 << 4) | (b2 >> 4);

        if d1 < FIELD_MODULUS && sampled < 16 {
            result[sampled] = d1;
            sampled += 1
        }
        if d2 < FIELD_MODULUS && sampled < 16 {
            result[sampled] = d2;
            sampled += 1
        }
    }
    sampled
}
