/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <vector>

struct X {
  ~X(){};
  void foo();
};

void unknown_init_ptr_by_ref(X** x);
void unknown_no_init_ptr(X* const* x); // cannot init because const

void init_ok() {
  X* p = nullptr;
  unknown_init_ptr_by_ref(&p);
  p->foo();
}

void const_no_init_bad() {
  X* p = nullptr;
  unknown_no_init_ptr(&p);
  p->foo();
}

void unknown_init_value_by_ref(X** x);

void wrap_unknown_init(X** x) { unknown_init_value_by_ref(x); }

void call_unknown_init_interproc_ok() {
  X* p = nullptr;
  wrap_unknown_init(&p);
  p->foo();
}

void unknown_with_pointer_formal(X* x);

void wrap_unknown_no_init(X* x) { unknown_with_pointer_formal(x); }

void call_init_with_pointer_value_bad() {
  X* p = nullptr;
  wrap_unknown_no_init(p);
  p->foo();
}

struct Queue {
  int size;
  bool empty() const;
  int front() const;
  void pop();
};

void drain_unknown_queue_bad(Queue& q, std::vector<int>& v) {
  v.push_back(0);
  int& ref = v[0];
  while (!q.empty()) {
    v.push_back(1);
    q.pop();
  }
  ref = 1;
}

int drain_unknown_queue_after_check_bad(Queue& q) {
  if (q.empty()) {
    return 0;
  }
  while (!q.empty()) {
    q.pop();
  }
  int* p = nullptr;
  return *p;
}

void empty_after_store_bad(Queue& q) {
  if (q.empty()) {
    q.size = 1;
    if (!q.empty()) {
      int* p = nullptr;
      *p = 1;
    }
  }
}

void empty_is_functional_ok(Queue& q) {
  int x = 0;
  int* p = nullptr;
  if (!q.empty()) {
    p = &x;
  }
  if (!q.empty()) {
    *p = 1;
  }
}

struct QueueHolder {
  Queue queue;
  int pending;
  bool has_work() const { return !queue.empty(); }
};

void has_work_is_functional_ok(QueueHolder& h) {
  int x = 0;
  int* p = nullptr;
  if (h.has_work()) {
    p = &x;
  }
  if (h.has_work()) {
    *p = 1;
  }
}

void has_work_after_unrelated_store_ok(QueueHolder& h) {
  int x = 0;
  int* p = nullptr;
  if (h.has_work()) {
    p = &x;
  }
  h.pending = 0;
  if (h.has_work()) {
    *p = 1;
  }
}

void drain(Queue& q) {
  while (!q.empty()) {
    q.pop();
  }
}

void empty_after_drain_ok(Queue& q) {
  drain(q);
  if (!q.empty()) {
    int* p = nullptr;
    *p = 1;
  }
}

struct UnknownIterator {
  bool operator!=(const UnknownIterator& other) const;
  UnknownIterator& operator++();
  int operator*() const;
};

struct UnknownRange {
  UnknownIterator begin();
  UnknownIterator end();
};

void range_for_unknown_iterator_bad(UnknownRange& r, std::vector<int>& v) {
  v.push_back(0);
  int& ref = v[0];
  for (int x : r) {
    v.push_back(x);
  }
  ref = 1;
}

int pop_front(Queue& q) {
  int x = q.front();
  q.pop();
  return x;
}

void pop_front_is_functional_ok(Queue& q) {
  int x = q.front();
  if (pop_front(q) != x) {
    int* p = nullptr;
    *p = 1;
  }
}

struct Pool {
  int size;
  int misses;
  bool empty() const;
  int* take() {
    if (empty()) {
      misses++;
      return nullptr;
    }
    size--;
    return &size;
  }
};

void take_after_check_ok(Pool& pool) {
  if (!pool.empty()) {
    *pool.take() = 1;
  }
}
