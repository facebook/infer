This error is reported in C++. It fires when the initialization of a static
variable `A`, accesses a static variable `B` from another translation unit
(usually another `.cpp` file). There are no guarantees whether `B` has been
already initialized or not at that point.

For more technical definition and techniques to avoid/remediate, see the
[FAQ](https://isocpp.org/wiki/faq/ctors#static-init-order).
