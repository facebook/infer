/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

char* a = "I'm a string";
char* b = "I'm a different string";
char* c = "foo bar";
char* d = "hello world";

void f();
void g();

void f() { // accesses: b, c, d
  char* s1 = b;
  g();
  char* s2 = c;
}

void g() { // accesses: b, c, d
  char* s = d;
  f();
}

int main() { // accesses: a,b,c,d
  if (__llair_choice()) {
    f();
  } else {
    g();
  }
  char* s = a;
  return 0;
}
