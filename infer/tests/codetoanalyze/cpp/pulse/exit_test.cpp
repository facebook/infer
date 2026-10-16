/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <pthread.h>
#include <stdlib.h>

#include <memory>

// we get two disjuncts one for each branch
void exit_positive(int a[10], int b) {
  if (b < 1) {
    exit(0);
  }
}

void unreachable_double_free_ok(int a[10], int b) {
  exit_positive(a, 0);
  free(a);
  free(a);
}

void store_exit(int* x, bool b) {
  if (b) {
    *x = 42;
    exit(0);
  }
}

void store_exit_null_bad() { store_exit(NULL, true); }

[[noreturn]] void fatal_error(const char* msg);

void new_then_noreturn_ok(bool b) {
  int* p = new int;
  if (b) {
    fatal_error("error");
  }
  delete p;
}

void new_leak_on_return_path_bad(bool b) {
  int* p = new int;
  if (b) {
    fatal_error("error");
  }
}

void unique_ptr_then_noreturn_ok(bool b) {
  std::unique_ptr<int> p(new int(42));
  if (b) {
    fatal_error("error");
  }
}

// with glibc, pthread_exit unwinds the stack of the thread and destroys p, but
// the path ends at the call
void FP_unique_ptr_then_pthread_exit_ok() {
  std::unique_ptr<int> p(new int(42));
  pthread_exit(nullptr);
}

struct Logger {
  [[noreturn]] void fatal(const char* msg);
  [[noreturn]] virtual void fatal_virtual(const char* msg);
};

void new_then_noreturn_method_ok(Logger& logger, bool b) {
  int* p = new int;
  if (b) {
    logger.fatal("error");
  }
  delete p;
}

void new_then_noreturn_virtual_method_ok(Logger* logger, bool b) {
  int* p = new int;
  if (b) {
    logger->fatal_virtual("error");
  }
  delete p;
}

[[noreturn]] void throw_error();

// the call is treated as the end of the program, but it can throw an exception
// that is caught here, outside the scope of p
void FN_new_then_noreturn_throw_caught_bad() {
  try {
    int* p = new int;
    throw_error();
  } catch (...) {
  }
}
