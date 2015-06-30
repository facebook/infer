int fun_default(int a = 3, int b = 5) {
  return a + b;
}

int fun_default_decl(int a, int b = 5);
// note that b is default param, but function was declared earlier
int fun_default_decl(int a, int b) {
  return a + b;
}

void test() {
  fun_default(1, 2);
  fun_default(1);
  fun_default();

  fun_default_decl(6);
  fun_default_decl(6,6);
}
