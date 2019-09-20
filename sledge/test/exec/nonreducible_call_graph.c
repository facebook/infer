/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdio.h>

char* a = "I'm a string";
char* b = "I'm a different string";
char* c = "foo bar";
char* d = "hello world";
FILE* file;

void f();
void g();

int main() {        // accesses: a,b,c,d
  if (getc(file)) { // nondeterministic
    f();
  } else {
    g();
  }
  char* s = a;
  return 0;
}

void f() { // accesses: b, c, d
  char* s1 = b;
  g();
  char* s2 = c;
}
void g() { // accesses: b, c, d
  char* s = d;
  f();
}
