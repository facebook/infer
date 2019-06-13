/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <istream>
#include <ostream>

namespace std {
extern std::istream cin;
extern std::wistream wcin;
extern std::ostream cerr;
extern std::wostream wcerr;
extern std::ostream clog;
extern std::wostream wclog;
extern std::ostream cout;
extern std::wostream wcout;
}; // namespace std

void printing_no_SIOF() {
  std::ios_base::Init ioInit;
  std::cerr << "it's ok to print here";
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

int global_std_cerr_access_bad = return_4_SIOF();
int global_std_cerr_access_good = return_4_no_SIOF();
