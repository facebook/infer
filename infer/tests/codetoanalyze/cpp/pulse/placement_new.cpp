/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstdlib>
#include <new>

namespace placement_new {

struct S {
  int f;
  S() : f(0) {}
};

enum class Tag {};

struct Pool {
  int used;
  void reset() { used = 0; }
};

struct PoolNode {
  int f;
  PoolNode() : f(0) {}
  static void* operator new(std::size_t size, Pool& pool);
  static void operator delete(void* p);
};

struct Arena {
  char storage[64];
};

struct ListNode;

struct List {
  ListNode* head = nullptr;
};

struct ListNode {
  ListNode* next;
  int v;
  ListNode(List& list, int x) : next(list.head), v(x) { list.head = this; }
};

struct InPlaceListNode : ListNode {
  InPlaceListNode(List& list, int x) : ListNode(list, x) {}
  static void* operator new(std::size_t size, void* p) noexcept { return p; }
};

struct SetsOutParam {
  explicit SetsOutParam(int* out) { *out = 1; }
};

struct Owner {
  char* buf;
  explicit Owner(char* b) : buf(b) {}
};

} // namespace placement_new

void* operator new(std::size_t size, placement_new::Tag, void* p) { return p; }
void* operator new(std::size_t size, void* p, placement_new::Tag) { return p; }
void* operator new(std::size_t size, int extra) { return malloc(size + extra); }
void* operator new(std::size_t size, const char* file, int line, int tag) {
  return malloc(size);
}
void* operator new(std::size_t size, placement_new::Arena& arena) noexcept;

namespace placement_new {

void storage_last_alias_bad() {
  S* s = new S();
  S* alias = new (s, Tag{}) S();
  delete s;
  alias->f = 1;
}

void storage_first_alias_bad() {
  S* s = new S();
  S* alias = new (Tag{}, s) S();
  delete s;
  alias->f = 1;
}

void storage_last_ok(void* mem) {
  S* s = new (mem, Tag{}) S();
  s->f = 1;
}

void extra_size_then_null_deref_bad() {
  S* s = new (20) S();
  int* p = nullptr;
  *p = s->f;
}

void three_args_then_null_deref_bad() {
  S* s = new ("file", 10, 3) S();
  int* p = nullptr;
  *p = s->f;
}

void three_args_last_arg_zero_ok() {
  S* s = new ("file", 10, 0) S();
  s->f = 1;
}

void class_operator_new_then_delete_ok(Pool& pool) {
  PoolNode* n = new (pool) PoolNode();
  delete n;
  pool.reset();
}

// The object is only initialized if the result of a noexcept allocation
// function is not null, but allocation functions that we do not see are assumed
// to succeed.
int noexcept_operator_new_ok(Arena& arena) {
  S* s = new (arena) S();
  s->f = 0;
  return s->f;
}

int noexcept_operator_new_registers_ok(Arena& arena) {
  List list;
  new (arena) ListNode(list, 1);
  return list.head->v;
}

int noexcept_operator_new_out_param_ok(Arena& arena) {
  int x;
  new (arena) SetsOutParam(&x);
  return x;
}

Owner* noexcept_operator_new_takes_ownership_ok(Arena& arena) {
  char* buf = (char*)malloc(10);
  if (buf == nullptr) {
    return nullptr;
  }
  return new (arena) Owner(buf);
}

// the storage is assumed not to be null unless it is known to be
int class_placement_stack_storage_ok() {
  alignas(InPlaceListNode) char storage[sizeof(InPlaceListNode)];
  List list;
  new (storage) InPlaceListNode(list, 1);
  return list.head->v;
}

int class_placement_param_storage_ok(void* storage) {
  List list;
  new (storage) InPlaceListNode(list, 1);
  return list.head->v;
}

int class_placement_malloc_unchecked_bad() {
  List list;
  InPlaceListNode* node =
      new (malloc(sizeof(InPlaceListNode))) InPlaceListNode(list, 1);
  int v = node->v;
  free(node);
  return v;
}

void construct_in(void* storage) { new (storage) S(); }

void construct_in_null_bad() { construct_in(nullptr); }

} // namespace placement_new
