This is reported when an address obtained via a non-zero constant is
dereferenced. If the address is zero then
[`NULLPTR_DEREFERENCE`](#nullptr_dereference) is reported instead.

For example, `int *p = (int *) 123; *p = 42;` generates this issue
type.
