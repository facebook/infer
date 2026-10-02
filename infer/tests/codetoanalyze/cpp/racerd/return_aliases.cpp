/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>
#include <mutex>

namespace return_aliases {

struct State {
  int x;
  void inc() { x++; }
};

class UniquePtrArrow {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    st_->x = v;
  }

  int get_bad() { return st_->x; }

 private:
  std::mutex mu_;
  std::unique_ptr<State> st_;
};

class SharedPtrArrow {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    st_->x = v;
  }

  int get_bad() { return st_->x; }

 private:
  std::mutex mu_;
  std::shared_ptr<State> st_;
};

class UniquePtrStar {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    (*st_).x = v;
  }

  int get_bad() { return (*st_).x; }

 private:
  std::mutex mu_;
  std::unique_ptr<State> st_;
};

class UniquePtrGet {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    st_.get()->x = v;
  }

  int get_bad() { return st_.get()->x; }

 private:
  std::mutex mu_;
  std::unique_ptr<State> st_;
};

class UniquePtrMethodCall {
 public:
  void locked_inc() {
    std::lock_guard<std::mutex> l(mu_);
    st_->inc();
  }

  void inc_bad() { st_->inc(); }

 private:
  std::mutex mu_;
  std::unique_ptr<State> st_;
};

class ReferenceAccessor {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    state().x = v;
  }

  int get_bad() { return state().x; }

 private:
  State& state() { return st_; }

  std::mutex mu_;
  State st_;
};

class PointerAccessorThroughUniquePtr {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    state()->x = v;
  }

  int get_bad() { return state()->x; }

 private:
  State* state() { return st_.get(); }

  std::mutex mu_;
  std::unique_ptr<State> st_;
};

State global_state;

State& get_global_state() { return global_state; }

class GlobalAccessor {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    get_global_state().x = v;
  }

  int get_bad() { return get_global_state().x; }

 private:
  std::mutex mu_;
};

class NullOnOnePath {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    state()->x = v;
  }

  int get_bad() { return state()->x; }

 private:
  State* state() {
    if (!ready_) {
      return nullptr;
    }
    return st_.get();
  }

  std::mutex mu_;
  bool ready_;
  std::unique_ptr<State> st_;
};

class LockedReferenceAccessor {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    st_.x = v;
  }

  int get_bad() { return state().x; }

 private:
  State& state() {
    std::lock_guard<std::mutex> l(mu_);
    return st_;
  }

  std::mutex mu_;
  State st_;
};

class AllLocked {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    st_->x = v;
  }

  int get_ok() {
    std::lock_guard<std::mutex> l(mu_);
    return st_->x;
  }

 private:
  std::mutex mu_;
  std::unique_ptr<State> st_;
};

class DifferentPointees {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    st1_->x = v;
  }

  int get_ok() { return st2_->x; }

 private:
  std::mutex mu_;
  std::unique_ptr<State> st1_;
  std::unique_ptr<State> st2_;
};

struct Node {
  Node* next;
  int v;
};

class ReassignedFormal {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    advance(head_)->v = v;
  }

  int get_ok() { return head_->v; }

 private:
  Node* advance(Node* p) {
    p = p->next;
    return p;
  }

  std::mutex mu_;
  Node* head_;
};

void next(Node** p) { *p = (*p)->next; }

class FormalAddressTaken {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    advance(head_)->v = v;
  }

  int get_ok() { return head_->v; }

 private:
  Node* advance(Node* p) {
    next(&p);
    return p;
  }

  std::mutex mu_;
  Node* head_;
};

class LockedGetter {
 public:
  void set(State* st) {
    std::lock_guard<std::mutex> l(mu_);
    st_ = st;
  }

  bool has_ok() { return get() != nullptr; }

 private:
  State* get() {
    std::lock_guard<std::mutex> l(mu_);
    return st_;
  }

  std::mutex mu_;
  State* st_;
};

class LocalUniquePtr {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    x_ = v;
  }

  int local_ok() {
    std::unique_ptr<State> p(new State());
    p->x = 1;
    return p->x;
  }

 private:
  std::mutex mu_;
  int x_;
};

class ConditionalAccessor {
 public:
  void set(bool b, int v) {
    std::lock_guard<std::mutex> l(mu_);
    pick(b).x = v;
  }

  // the accessor returns one of two fields, so the access through its result
  // is not resolved
  int FN_get_bad(bool b) { return pick(b).x; }

 private:
  State& pick(bool b) {
    if (b) {
      return st1_;
    }
    return st2_;
  }

  std::mutex mu_;
  State st1_;
  State st2_;
};

class Holder {
 public:
  State& state() { return st_; }

 private:
  State st_;
};

class AccessorOnLocal {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    h_.state().x = v;
  }

  int accessor_on_local_ok() {
    Holder h;
    h.state().x = 1;
    return h.state().x;
  }

 private:
  std::mutex mu_;
  Holder h_;
};

struct VBase {
  virtual ~VBase() {}
  virtual State* getp() { return p1_; }

  State* p1_;
};

struct VDerived : public VBase {
  State* getp() override { return p2_; }

  State* p2_;
};

class VirtualAccessor {
 public:
  VirtualAccessor() : b_(new VDerived()) {}

  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    b_->p1_->x = v;
  }

  // the call is resolved to [VBase::getp()], which returns [p1_], but the
  // override in [VDerived], the dynamic type of [b_], returns [p2_]
  int FP_read_via_virtual_accessor_ok() { return b_->getp()->x; }

 private:
  std::mutex mu_;
  VBase* b_;
};

thread_local State thread_state_;

State* thread_state() { return &thread_state_; }

class ThreadLocalAccessor {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    thread_state()->x = v;
  }

  // each thread has its own [thread_state_], but thread-local variables are
  // treated as globals
  int FP_thread_local_accessor_ok() { return thread_state()->x; }

 private:
  std::mutex mu_;
};

} // namespace return_aliases
