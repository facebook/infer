/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <string>

class SomeStructA {

 public:
  int* ptr;
  std::string a_name;
  std::string* p_a_name;
  int i;

  SomeStructA() {} // user defined default constructor.
  // Does not initialize i so, it gets random value
};

class SomeStructB {

 public:
  std::string a_name;
  int i;

  // No user defined default constructor.
  // If default constructor is called i is initialized.
};

int access_members_bad() {
  SomeStructA a;

  std::string n = a.a_name;
  int i = a.i; // error

  return 0;
};

int access_members_bad2() {
  SomeStructA a{};

  std::string n = a.a_name;
  int i = a.i; // error

  return 0;
};

int access_members_ok() {

  SomeStructB b{}; // call default implicit constructor which initialize i.
  std::string n = b.a_name;
  int i = b.i;

  return 0;
};

int access_members_bad3() {

  SomeStructB b; // no constructor is called
  std::string n = b.a_name;
  int i = b.i;

  return 0;
};

int access_pointer_members_bad() {
  SomeStructA a;

  int* p = a.ptr;
  int i = a.i;
  std::string* pn = a.p_a_name;

  return 0;
};
