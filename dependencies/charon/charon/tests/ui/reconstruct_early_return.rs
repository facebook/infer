fn f() -> usize
{
    let mut i = 0;
    let mut j = 0;
    while i < 32
    {
        j += 1;
        if j > 16
        {
            j /= 2;
        }
        else if j > 32
        {
            return 1;
        }
        i += 1;
    }

    0
}
