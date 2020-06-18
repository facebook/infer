Buffer overrun reports fall into several "buckets" corresponding to the expected precision of the
report.  The higher the number, the more likely it is to be a false positive.

*   `L1`: The most faithful report, when it *must* be unsafe.  For example, array size: `[5,5]`,
    offset: `[3,3]`.

*   `L2`: Less faithful report than `L1`, when it *may* be unsafe.  For example, array size:`[5,5]`,
    offset: `[0,5]`.  Note that the offset may be a safe value in the real execution, i.e. 0, 1, 2,
    3, 4.

*   `L5`: The least faithful report, when there is an interval top.  For example, array size:
    `[5,5]`, offset: `[-oo,+oo]`.

*   `L4`: More faithful report than `L5`, when there is an infinity value.  For example, array size:
    `[5,5]`, offset: `[0, +oo]`.

*   `L3`: The reports that are not included in the above cases.

*   `S2`: An array access is unsafe by symbolic values.  For example, array size: `[n,n]`, offset
    `[n,+oo]`.

*   `U5`: An array access is unsafe by unknown values, which are usually from unknown function
    calls.
