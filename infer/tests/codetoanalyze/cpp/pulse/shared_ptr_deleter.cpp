/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstdio>
#include <cstdlib>
#include <memory>
#include <vector>

namespace shared_ptr_deleter {

// unlike unique_ptr, shared_ptr also calls its deleter on a null pointer, and
// fclose(nullptr) is undefined: the tests check the result of fopen first
void function_pointer_deleter_ok(const char* path) {
  if (FILE* file = fopen(path, "r")) {
    std::shared_ptr<FILE> f(file, fclose);
  }
}

void null_pointer_deleter_called_bad() {
  std::shared_ptr<int> p(nullptr, [](int* q) { *q = 0; });
}

void free_deleter_ok() {
  std::shared_ptr<int> p((int*)malloc(sizeof(int)), free);
}

struct FileCloser {
  void operator()(FILE* f) const { fclose(f); }
};

void functor_deleter_ok(const char* path) {
  if (FILE* file = fopen(path, "r")) {
    std::shared_ptr<FILE> f(file, FileCloser());
  }
}

struct FreeDeleter {
  template <typename T>
  void operator()(T* p) const {
    free(p);
  }
};

void template_functor_deleter_ok() {
  std::shared_ptr<int> p((int*)malloc(sizeof(int)), FreeDeleter());
}

void lambda_deleter_ok(const char* path) {
  if (FILE* file = fopen(path, "r")) {
    std::shared_ptr<FILE> f(file, [](FILE* f) { fclose(f); });
  }
}

void reference_lambda_deleter_ok() {
  std::shared_ptr<char> p((char*)malloc(1), [](char* const& p) { free(p); });
}

void noop_lambda_deleter_leak_bad() {
  std::shared_ptr<int> p(new int(1), [](int*) {});
}

// deleters with state are unknown calls, which may release the pointer
void FN_capturing_lambda_deleter_leak_bad() {
  int calls = 0;
  std::shared_ptr<int> p(new int(1), [&calls](int*) { calls++; });
}

void copied_deleter_ok(const char* path) {
  if (FILE* file = fopen(path, "r")) {
    std::shared_ptr<FILE> f(file, fclose);
    std::shared_ptr<FILE> g = f;
  }
}

void reset_with_deleter_ok(const char* path) {
  std::shared_ptr<FILE> f;
  f.reset(fopen(path, "r"), fclose);
}

void reset_non_empty_with_deleter_ok(const char* path) {
  std::shared_ptr<FILE> f(fopen(path, "r"), fclose);
  f.reset(fopen(path, "w"), fclose);
}

void reset_file(std::shared_ptr<FILE>& f) { f.reset(); }

void reset_in_callee_ok(const char* path) {
  if (FILE* file = fopen(path, "r")) {
    std::shared_ptr<FILE> f(file, fclose);
    reset_file(f);
  }
}

struct FileHolder {
  std::shared_ptr<FILE> f;
  explicit FileHolder(FILE* file) : f(file, fclose) {}
};

void member_deleter_ok(const char* path) {
  if (FILE* file = fopen(path, "r")) {
    FileHolder h(file);
  }
}

void close_twice_bad(const char* path) {
  if (FILE* file = fopen(path, "r")) {
    std::shared_ptr<FILE> f(file, fclose);
    fclose(f.get());
  }
}

struct NoopDeleter {
  void operator()(int*) const {}
};

void noop_deleter_leak_bad() {
  std::shared_ptr<int> p(new int(1), NoopDeleter());
}

int non_owning_ok() {
  int x = 0;
  { std::shared_ptr<int> p(&x, NoopDeleter()); }
  return x;
}

struct View {
  std::shared_ptr<int> p;
  explicit View(int* x) : p(x, NoopDeleter()) {}
};

int non_owning_member_ok() {
  int x = 0;
  { View v(&x); }
  return x;
}

void capturing_lambda_deleter_ok(std::vector<FILE*>& cache, const char* path) {
  std::shared_ptr<FILE> f(fopen(path, "r"),
                          [&cache](FILE* f) { cache.push_back(f); });
}

void flag_lambda_deleter_ok(const char* path) {
  bool close = true;
  std::shared_ptr<FILE> f(fopen(path, "r"), [close](FILE* f) {
    if (close) {
      fclose(f);
    }
  });
}

} // namespace shared_ptr_deleter
