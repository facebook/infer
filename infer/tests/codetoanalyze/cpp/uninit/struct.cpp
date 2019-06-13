/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <string>

struct SimpleRuleKey {
  std::string value;
  int i;
};

struct s {
  int n;
  int b;
  std::string s;
};

struct s t4; // global struc are init by default

std::string struct_init_ok() {
  SimpleRuleKey srk{};
  std::string s1;
  s1 = srk.value; // srk has been initialized

  return s1;
}

std::string struct_uninit_ok() {
  SimpleRuleKey srk;
  std::string s1;
  s1 = srk.value; // srk has not been initialized, but value has type string and
                  // therefore it is initialized by default

  return s1;
}

int struct_uninit_bad() {
  int k;
  SimpleRuleKey srk;
  k = srk.i; // Should reports here: srk was not initialized and i has type int
             // so it's not initialized by default
  return k;
}

int struct_partially_init_ok() {

  struct s t1 = {0}; // partially initialized
  int j;
  j = t1.b; // when partially initialized, automatically other fields get
            // initilized

  return j;
}

int global_struct_ok() {

  int j;
  j = t4.n; // global struct are initilized by default

  return j;
}

void init_struct(struct s* z) { z->n = 10; };

int call_init_struct_ok() {
  struct s t;
  init_struct(&t);

  return t.n;
}

int FN_call_init_struct() {
  struct s t;
  init_struct(&t);

  return t.b;
}

struct s init_all_fields(void);

int init_field_via_function_ok() {

  struct s t;

  t = init_all_fields();
  return t.n;
}

int init_field_via_function_ptr_ok() {

  struct s* t;

  *t = init_all_fields();
  return t->n;
}

int get_a_int_pointer(int* x) { return *x; };

void pass_pointer_of_field_OK() {
  struct s my_t;

  get_a_int_pointer(&my_t.n);
}

int get_an_int(int x){};

void pass_basic_type_field_bad() {
  struct s my_t;

  get_an_int(my_t.n); // pass an uninit int
}

enum class FieldType : uint8_t {
  Stop = 0x0,
  True = 0x1,
  False = 0x2,
  Int8 = 0x3,
  Int16 = 0x4,
  Int32 = 0x5,
  Int64 = 0x6,
  Double = 0x7,
  Binary = 0x8,
  List = 0x9,
  Set = 0xa,
  Map = 0xb,
  Struct = 0xc,
  Float = 0xd
};

class C {
  int a, b;

 public:
  std::pair<FieldType, int> read_values();
};

int use_C(C& c) {
  const auto pr = c.read_values();

  return pr.second;
}

struct s not_a_constructor_but_returning_T(void);

int foo() {
  struct s t = not_a_constructor_but_returning_T();
  return t.n;
}

short struct_partial_init_bad() {
  struct {
    int* a;
    short* b;
  } s;
  s.a = 0;
  short* p = s.b;
  return *p;
}
