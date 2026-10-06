/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Declared here rather than taken from <new>: the expected output records
// whether std::nothrow's type is POD, and the defaulted constructor that <new>
// gives it makes it non-POD on some targets (e.g. Darwin) only.
namespace std {
struct nothrow_t {};
extern const nothrow_t nothrow;
} // namespace std

void* operator new(decltype(sizeof(0)), const std::nothrow_t&) noexcept;
void* operator new[](decltype(sizeof(0)), const std::nothrow_t&) noexcept;
void* operator new(decltype(sizeof(0)), void*) noexcept;

struct Obj {
  int x;
  Obj(int v) : x(v) {}
};

int* nothrow_scalar() { return new (std::nothrow) int(1); }

Obj* nothrow_ctor() { return new (std::nothrow) Obj(2); }

int* nothrow_array_no_init(unsigned n) { return new (std::nothrow) int[n]; }

Obj* throwing_ctor() { return new Obj(3); }

Obj* reserved_placement(void* buf) { return new (buf) Obj(4); }

struct Resource {
  ~Resource();
};

struct Holder {
  Holder(const Resource& r);
};

Holder* nothrow_temporary_in_initializer() {
  return new (std::nothrow) Holder(Resource());
}
