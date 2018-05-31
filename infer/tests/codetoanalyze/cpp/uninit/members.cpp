/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <string>

class A {

 public:
  int* ptr;
  std::string a_name;
  std::string* p_a_name;
  int i;

  A() {} // user defined default constructor.
         // Does not initialize i so, it gets random value
};

class B {

 public:
  std::string a_name;
  int i;

  // No user defined default constructor.
  // If default constructor is called i is initialized.
};

int access_members_bad() {
  A a;

  std::string n = a.a_name;
  int i = a.i; // error

  return 0;
};

int access_members_bad2() {
  A a{};

  std::string n = a.a_name;
  int i = a.i; // error

  return 0;
};

int access_members_ok() {

  B b{}; // call default implicit constructor which initialize i.
  std::string n = b.a_name;
  int i = b.i;

  return 0;
};

int access_members_bad3() {

  B b; // no constructor is called
  std::string n = b.a_name;
  int i = b.i;

  return 0;
};

int access_pointer_members_bad() {
  A a;

  int* p = a.ptr;
  int i = a.i;
  std::string* pn = a.p_a_name;

  return 0;
};
