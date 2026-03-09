int g(int x);
int h(int x);

int f(int x) {
  if (x <= 0) return 1;
  return g(x - 1) + h(x - 2) + g(x - 3) + f(x - 4);
}

int g(int x) {
  if (x <= 0) return 2;
  return f(x - 1) + h(x - 2) + g(x - 3);
}

int h(int x) {
  if (x <= 0) return 3;
  return f(x - 1) + g(x - 2) + h(x - 3);
}
