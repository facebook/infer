/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstdio>
#include <cstdlib>
#include <functional>
#include <memory>
#include <vector>

namespace unique_ptr_deleter {

using unique_file = std::unique_ptr<FILE, decltype(&fclose)>;

int function_pointer_deleter_ok(const char* path) {
  unique_file f(fopen(path, "r"), fclose);
  if (!f) {
    return -1;
  }
  return fgetc(f.get());
}

int function_pointer_type_deleter_ok(const char* path) {
  std::unique_ptr<FILE, int (*)(FILE*)> f(fopen(path, "r"), &fclose);
  if (f == nullptr) {
    return -1;
  }
  return fgetc(f.get());
}

void free_deleter_ok() {
  std::unique_ptr<int, decltype(&free)> p((int*)malloc(sizeof(int)), free);
}

struct FileCloser {
  void operator()(FILE* f) const { fclose(f); }
};

int functor_deleter_ok(const char* path) {
  std::unique_ptr<FILE, FileCloser> f(fopen(path, "r"));
  if (!f) {
    return -1;
  }
  return fgetc(f.get());
}

void functor_deleter_argument_ok(const char* path) {
  std::unique_ptr<FILE, FileCloser> f(fopen(path, "r"), FileCloser());
}

struct Closer {
  void operator()(FILE* f) const { fclose(f); }
  void operator()(int* p) const { delete p; }
};

void overloaded_functor_deleter_ok(const char* path) {
  std::unique_ptr<FILE, Closer> f(fopen(path, "r"));
  std::unique_ptr<int, Closer> p(new int(1));
}

struct FreeDeleter {
  template <typename T>
  void operator()(T* p) const {
    free(p);
  }
};

void template_functor_deleter_ok() {
  std::unique_ptr<int, FreeDeleter> p((int*)malloc(sizeof(int)));
}

void lambda_deleter_ok(const char* path) {
  auto closer = [](FILE* f) { fclose(f); };
  std::unique_ptr<FILE, decltype(closer)> f(fopen(path, "r"), closer);
}

void noop_lambda_deleter_leak_bad() {
  auto keep = [](int*) {};
  std::unique_ptr<int, decltype(keep)> p(new int(1), keep);
}

void capturing_lambda_deleter_ok(std::vector<FILE*>& cache, const char* path) {
  auto keep = [&cache](FILE* f) { cache.push_back(f); };
  std::unique_ptr<FILE, decltype(keep)> f(fopen(path, "r"), keep);
}

struct MaybeCloser {
  bool close = true;
  void operator()(FILE* f) const {
    if (close) {
      fclose(f);
    }
  }
};

void stateful_functor_deleter_ok(const char* path) {
  std::unique_ptr<FILE, MaybeCloser> f(fopen(path, "r"));
}

// deleters with state (fields, captures, a type-erased callable) are unknown
// calls, which may release the pointer
void FN_stateful_functor_deleter_leak_bad(const char* path) {
  std::unique_ptr<FILE, MaybeCloser> f(fopen(path, "r"));
  f.get_deleter().close = false;
}

void FN_capturing_lambda_deleter_leak_bad() {
  int calls = 0;
  auto count = [&calls](int*) { calls++; };
  std::unique_ptr<int, decltype(count)> p(new int(1), count);
}

void std_function_deleter_ok(const char* path) {
  std::unique_ptr<FILE, std::function<void(FILE*)>> f(
      fopen(path, "r"), [](FILE* f) { fclose(f); });
}

void FN_noop_std_function_deleter_leak_bad() {
  std::unique_ptr<int, std::function<void(int*)>> p(new int(1), [](int*) {});
}

struct ReferenceCloser {
  void operator()(FILE* const& f) const { fclose(f); }
};

void reference_functor_deleter_ok(const char* path) {
  std::unique_ptr<FILE, ReferenceCloser> f(fopen(path, "r"));
}

void reference_lambda_deleter_ok() {
  auto release = [](char* const& p) { free(p); };
  std::unique_ptr<char, decltype(release)> p((char*)malloc(1), release);
}

int reference_deleter_use_after_delete_bad() {
  int* raw = new int(1);
  {
    auto destroy = [](int* const& p) { delete p; };
    std::unique_ptr<int, decltype(destroy)> p(raw, destroy);
  }
  return *raw;
}

void null_pointer_deleter_not_called_ok() {
  auto deref = [](int* p) { *p = 0; };
  std::unique_ptr<int, decltype(deref)> p(nullptr, deref);
}

unique_file open_file(const char* path) {
  return unique_file(fopen(path, "r"), fclose);
}

int returned_deleter_ok(const char* path) {
  unique_file f = open_file(path);
  if (!f) {
    return -1;
  }
  return fgetc(f.get());
}

void reset_file(unique_file& f) { f.reset(); }

void reset_in_callee_ok(const char* path) {
  unique_file f(fopen(path, "r"), fclose);
  reset_file(f);
}

void reset_in_callee_then_close_bad(const char* path) {
  FILE* raw = fopen(path, "r");
  if (raw) {
    unique_file f(raw, fclose);
    reset_file(f);
    fclose(raw);
  }
}

void reset_with_deleter_ok(const char* path) {
  unique_file f(fopen(path, "r"), fclose);
  f.reset(fopen(path, "w"));
}

FILE* release_ok(const char* path) {
  unique_file f(fopen(path, "r"), fclose);
  return f.release();
}

void release_leak_bad(const char* path) {
  unique_file f(fopen(path, "r"), fclose);
  f.release();
}

void get_deleter_ok(const char* path) {
  unique_file f(fopen(path, "r"), fclose);
  if (f) {
    f.get_deleter()(f.release());
  }
}

void close_twice_bad(const char* path) {
  unique_file f(fopen(path, "r"), fclose);
  if (f) {
    fclose(f.get());
  }
}

int keep_open(FILE*) { return 0; }

void moved_deleter_ok(const char* path) {
  unique_file f(fopen(path, "r"), fclose);
  unique_file g = std::move(f);
}

void move_assigned_deleter_leak_bad(const char* path) {
  unique_file f(fopen(path, "r"), keep_open);
  unique_file g(nullptr, fclose);
  g = std::move(f);
}

void swapped_deleter_leak_bad(const char* path) {
  unique_file f(fopen(path, "r"), keep_open);
  unique_file g(nullptr, fclose);
  f.swap(g);
}

struct NoopDeleter {
  void operator()(int*) const {}
};

void noop_deleter_leak_bad() {
  std::unique_ptr<int, NoopDeleter> p(new int(1));
}

struct DeleteDeleter {
  void operator()(int* p) const { delete p; }
};

void delete_deleter_ok() { std::unique_ptr<int, DeleteDeleter> p(new int(1)); }

int delete_deleter_use_after_delete_bad() {
  int* raw = new int(1);
  { std::unique_ptr<int, DeleteDeleter> p(raw); }
  return *raw;
}

} // namespace unique_ptr_deleter
