typedef int (*fun_ptr)(int, int);

int add(int x, int y) { return x + y; }

int mul(int x, int y) { return x * y; }

int invoke0(fun_ptr g, int x) { return g(x, 0); }

int invoke1(fun_ptr g, int x) { return g(x, 1); }

int apply(int (*f)(fun_ptr, int), fun_ptr g, int a) { return f(g, a); }

int test_add_0() { return apply(invoke0, add, 3); }

int test_add_1() { return apply(invoke1, add, 3); }

int test_mul_0() { return apply(invoke0, mul, 3); }

int test_mul_1() { return apply(invoke1, mul, 3); }
