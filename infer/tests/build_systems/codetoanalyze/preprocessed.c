# 1 "/tmp/removed_src.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 330 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/tmp/removed_src.c" 2

# 1 "/tmp/removed_header.h" 1

void fun();
# 3 "/tmp/removed_src.c" 2

int deref(int* a) { return *a; }

int test() { return deref(0); }
