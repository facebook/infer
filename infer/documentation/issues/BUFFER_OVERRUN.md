This is reported when outside of buffer bound is accessed.  It can corrupt memory and may introduce
security issues in C/C++.

For example, `int a[3]; a[5] = 42;` generates a `BUFFER_OVERRUN_L1` on `a[5] = 42;`.

Buffer overrun reports fall into several "buckets" corresponding to the expected precision of the
report.  The higher the number, the more likely it is to be a false positive.

*   `L1`: The most faithful report, when it *must* be unsafe.  For example, array size: `[3,3]`,
    offset: `[5,5]`.

*   `L2`: Less faithful report than `L1`, when it *may* be unsafe.  For example, array size:`[3,3]`,
    offset: `[0,5]`.  Note that the offset may be a safe value in the real execution, i.e. safe when
    0, 1, or 2; unsafe when 3, 4, or 5.

*   `L5`: The least faithful report, when there is an interval top.  For example, array size:
    `[3,3]`, offset: `[-oo,+oo]`.

*   `L4`: More faithful report than `L5`, when there is an infinity value.  For example, array size:
    `[3,3]`, offset: `[0, +oo]`.

*   `L3`: The reports that are not included in the above cases.

*   `S2`: An array access is unsafe by symbolic values.  For example, array size: `[n,n]`, offset
    `[n,+oo]`.

*   `U5`: An array access is unsafe by unknown values, which are usually from unknown function
    calls.
