/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <iostream>

void printing_no_SIOF() {
  std::ios_base::Init ioInit;
  std::cerr << "I haz warning!";
}

void printing_SIOF() {
  std::cerr << "I forgot to initialize std::cerr so I might not see this error "
               "message!";
}

int return_4_SIOF() {
  printing_SIOF();
  return 4;
}

void print_from_function_call() { std::cout << "I can even print here"; }

int return_4_no_SIOF() {
  printing_no_SIOF();
  std::clog << "I can even print here with clog";
  std::wclog << "I can even print here with wclog";
  std::cout << "I can even print here with cout";
  std::wcout << "I can even print here with wcout";
  std::cerr << "I can even print here with cerr";
  std::wcerr << "I can even print here with wcerr";
  int x;
  std::cin >> x;
  std::wcin >> x;
  print_from_function_call();
  return 4;
}

int global_bad_std_cerr_access = return_4_SIOF();
int global_good_std_cerr_access = return_4_no_SIOF();
