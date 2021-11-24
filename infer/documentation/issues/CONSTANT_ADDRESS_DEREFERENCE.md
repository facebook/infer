This is reported when an address at an absolute location, e.g. 1234,
is dereferenced. It is a more general version of the
[`NULLPTR_DEREFERENCE`](#nullptr_dereference) error type that is
reported when the address is a constant other than zero.

For example, `int *p = (int *) 123; *p = 42;` generates a `CONSTANT_ADDRESS_DEREFERENCE` on `*p = 42;`.

For more information see the [`NULLPTR_DEREFERENCE`](#nullptr_dereference) issue type.
