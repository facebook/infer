/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include "my_lib.h"
#include <stdlib.h>
#include <assert.h>

int main() { return 0; }
int* test() { return NULL; }

int add(int n1, int n2) { return n1 + n2; }

int test2() {
  int n1;
  int n2;
  return n1 + n2;
}

int test3(int* ptr) { return *ptr; }

int foo();

int test4() { return foo(); }

int test5() { return bar(); }

int call_add() { return add(1, 2); }

int test6() {
  int* ptr = NULL;
  if (call_add() == 3) {
    return *ptr;
  } else
    return 0;
}

int test7() {
  int* ptr = NULL;
  if (call_add() == 5) {
    return *ptr;
  } else
    return 0;
}

int call_add1(int* ptr, int y) { return add(*ptr, y); }

int test8(int* ptr) {
  *ptr = 1;
  int* null_ptr = NULL;
  int ret = call_add1(ptr, 2);
  if (ret == 3) {
    return *null_ptr;
  } else
    return 0;
}

int test9() {
  int* ptr = malloc(sizeof(int));
  *ptr = 42;
  int ret = *ptr;
  free(ptr);
  return ret;
}

int test10() {
  int sum = 0;
  for (int i = 0; i < 3; i++) {
    sum += i;
  }
  assert(sum != 3);
  return sum;
}

void test11() {}

void my_fun(const char* str);

void test12() {
  my_fun("hello");
  const char* str;
  my_fun(str);
}

int test13() {
  int i = 10;
  while (i >= 8) {
    i--;
  }
  assert(i != 7);
  return i;
}

struct Cat {
  char* name;
  int age;
};

struct Person {
  char* name;
  char* address;
  int age;
  int height;
  struct Cat* cat;
};

int get_age(struct Person* person) { return person->age; }

int test14() {
  struct Person* person = malloc(sizeof(struct Person));
  if (person) {
    person->age = 30;
    person->name = "John";
    int age = get_age(person);
    free(person);
    assert(age != 30);
    return age;
  }
  return 0;
}

int test15() {
  struct Person person = {.age = 30, .name = "John"};
  int age = get_age(&person);
  assert(age != 30);
  return age;
}

int test16(struct Person* person, struct Cat* cat) {
  cat->age = 10;
  person->age = 30;
  person->name = "John";
  person->cat = cat;
  int age = person->cat->age;
  assert(age != 10);
  return age;
}

int test17(int b) {
  int z = b ? 1 : 0;
  return z;
}

int deref_example(int* ptr) { return *ptr; }

int npe_bad() { return deref_example(NULL); }
