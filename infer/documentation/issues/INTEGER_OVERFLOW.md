This is reported when integer overflow occurred by integer operations such as addition, subtraction,
and multiplication. For example, `int n = INT_MAX; int m = n + 3;` generates a INTEGER_OVERFLOW_L1
on `n + 3`.

Integer overflows reports fall into several "buckets" corresponding to the expected precision of the
report. The higher the number, the more likely it is to be a false positive.

*   `L1`: The most faithful report, when it *must* be unsafe.  For example,
    `[2147483647,2147483647] + [1,1]` in 32-bit signed integer type.

*   `L2`: Less faithful report than `L1`, when it *may* be unsafe.  For example,
    `[2147483647,2147483647] + [0,1]` in 32-bit signed integer type.  Note that the integer of RHS
    can be 0, which is safe.

*   `L5`: The reports that are not included in the above cases.

*   `U5`: A binary integer operation is unsafe by unknown values, which are usually from unknown
    function calls.
