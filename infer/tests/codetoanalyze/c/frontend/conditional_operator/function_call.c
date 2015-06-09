/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

void some_f(int, int, int);

void fun_ifthenelse1() {
  (1 ? some_f : some_f)(1, 2, 3);
}

void fun_ifthenelse2() {
  (1 ? some_f : some_f)(0 ? 1 : 1, 0 ? 2 : 2, 0 ? 3 : 3);
}

void fun_ifthenelse3() {
  some_f(0 ? 1 : 1, 0 ? 2 : 2, 0 ? 3 : 3);
}

void fun_ifthenelse4() {
  (1 ? some_f : some_f)(0 ? 1 : 1, 2, 0 ? 3 : 3);
}
