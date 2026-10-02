/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace member_locks {

class MemberB;

class MemberA {
 public:
  // deadlock between lock_then_call_b_bad() and MemberB::lock_then_call_a_bad()
  void lock_then_call_b_bad();
  void lock_a() { std::lock_guard<std::mutex> l(mutex_); }

  MemberB* b_;

 private:
  std::mutex mutex_;
};

class MemberB {
 public:
  void lock_b() { std::lock_guard<std::mutex> l(mutex_); }
  void lock_then_call_a_bad() {
    std::lock_guard<std::mutex> l(mutex_);
    a_->lock_a();
  }

  MemberA* a_;

 private:
  std::mutex mutex_;
};

void MemberA::lock_then_call_b_bad() {
  std::lock_guard<std::mutex> l(mutex_);
  b_->lock_b();
}

class OrderedB;

class OrderedA {
 public:
  // both classes take the lock of OrderedA first
  void lock_then_call_b_ok();

  OrderedB* b_;

 private:
  std::mutex mutex_;
};

class OrderedB {
 public:
  void lock_b() { std::lock_guard<std::mutex> l(mutex_); }
  void call_a_ok() { a_->lock_then_call_b_ok(); }

  OrderedA* a_;

 private:
  std::mutex mutex_;
};

void OrderedA::lock_then_call_b_ok() {
  std::lock_guard<std::mutex> l(mutex_);
  b_->lock_b();
}

class Owner;

class Part {
 public:
  void lock_part() { std::lock_guard<std::mutex> l(mutex_); }
  // deadlock between lock_then_call_owner_bad() and
  // Owner::lock_then_call_part_bad()
  void lock_then_call_owner_bad();

  Owner* owner_;

 private:
  std::mutex mutex_;
};

class Owner {
 public:
  void lock_owner() { std::lock_guard<std::mutex> l(mutex_); }
  void lock_then_call_part_bad() {
    std::lock_guard<std::mutex> l(mutex_);
    part_.lock_part();
  }

  Part part_;

 private:
  std::mutex mutex_;
};

void Part::lock_then_call_owner_bad() {
  std::lock_guard<std::mutex> l(mutex_);
  owner_->lock_owner();
}

class ChainC;

class ChainA {
 public:
  void lock_a() { std::lock_guard<std::mutex> l(mutex_); }
  // deadlock between lock_then_call_c_bad() and ChainC::lock_then_call_a_bad()
  void lock_then_call_c_bad();

  struct Link {
    ChainC* c_;
  };
  Link* link_;

 private:
  std::mutex mutex_;
};

class ChainC {
 public:
  void lock_c() { std::lock_guard<std::mutex> l(mutex_); }
  void lock_then_call_a_bad() {
    std::lock_guard<std::mutex> l(mutex_);
    a_->lock_a();
  }

  ChainA* a_;

 private:
  std::mutex mutex_;
};

void ChainA::lock_then_call_c_bad() {
  std::lock_guard<std::mutex> l(mutex_);
  link_->c_->lock_c();
}

class RefB;

class RefA {
 public:
  // deadlock between lock_then_call_b_bad() and RefB::lock_then_call_a_bad()
  void lock_then_call_b_bad(RefB& b);
  void lock_a() { std::lock_guard<std::mutex> l(mutex_); }

 private:
  std::mutex mutex_;
};

class RefB {
 public:
  void lock_b() { std::lock_guard<std::mutex> l(mutex_); }
  void lock_then_call_a_bad(RefA& a) {
    std::lock_guard<std::mutex> l(mutex_);
    a.lock_a();
  }

 private:
  std::mutex mutex_;
};

void RefA::lock_then_call_b_bad(RefB& b) {
  std::lock_guard<std::mutex> l(mutex_);
  b.lock_b();
}

class HeldB;

class HeldA {
 public:
  void lock() { mutex_.lock(); }
  void unlock() { mutex_.unlock(); }
  // deadlock between lock_b_then_self_bad() and HeldB::lock_a_then_self_bad()
  void lock_b_then_self_bad();

  HeldB* b_;

 private:
  std::mutex mutex_;
};

class HeldB {
 public:
  void lock() { mutex_.lock(); }
  void unlock() { mutex_.unlock(); }
  void lock_a_then_self_bad() {
    a_->lock();
    {
      std::lock_guard<std::mutex> l(mutex_);
    }
    a_->unlock();
  }

  HeldA* a_;

 private:
  std::mutex mutex_;
};

void HeldA::lock_b_then_self_bad() {
  b_->lock();
  {
    std::lock_guard<std::mutex> l(mutex_);
  }
  b_->unlock();
}

class TreeNode {
 public:
  // deadlock between parent_then_self_bad() and self_then_parent_bad()
  void parent_then_self_bad() {
    std::lock_guard<std::mutex> l1(parent_->mutex_);
    std::lock_guard<std::mutex> l2(mutex_);
  }

  void self_then_parent_bad() {
    std::lock_guard<std::mutex> l2(mutex_);
    std::lock_guard<std::mutex> l1(parent_->mutex_);
  }

  // deadlock with self_then_parent_bad() on the child, but a node reached
  // through fields is not matched with `this` of another node
  void FN_self_then_child_bad() {
    std::lock_guard<std::mutex> l1(mutex_);
    std::lock_guard<std::mutex> l2(child_->mutex_);
  }

 private:
  TreeNode* parent_;
  TreeNode* child_;
  std::mutex mutex_;
};

class ListNode {
 public:
  // both methods lock a node before the next one
  void self_then_next_ok() {
    std::lock_guard<std::mutex> l1(mutex_);
    std::lock_guard<std::mutex> l2(next_->mutex_);
  }

  void next_then_next_next_ok() {
    std::lock_guard<std::mutex> l1(next_->mutex_);
    std::lock_guard<std::mutex> l2(next_->next_->mutex_);
  }

 private:
  ListNode* next_;
  std::mutex mutex_;
};

class Shared {
 public:
  void lock() { mutex_.lock(); }
  void unlock() { mutex_.unlock(); }

 private:
  std::mutex mutex_;
};

class ThirdPartyY;

// the paths of the lock of Shared in X and Y are only matched against a path
// rooted at Shared
class ThirdPartyX {
 public:
  void FN_lock_shared_then_y_bad();

  Shared* shared_;
  ThirdPartyY* y_;
};

class ThirdPartyY {
 public:
  void lock_y() { std::lock_guard<std::mutex> l(mutex_); }
  void FN_lock_y_then_shared_bad() {
    std::lock_guard<std::mutex> l(mutex_);
    shared_->lock();
    shared_->unlock();
  }

  Shared* shared_;

 private:
  std::mutex mutex_;
};

void ThirdPartyX::FN_lock_shared_then_y_bad() {
  shared_->lock();
  y_->lock_y();
  shared_->unlock();
}

class GatedB;

// both classes first take the lock of the same Shared object, but the paths of
// that lock in the two classes are not matched, so it is not a common lock
class GatedA {
 public:
  void lock_a() { std::lock_guard<std::mutex> l(mutex_); }
  void FP_lock_shared_then_call_b_ok();

  Shared* shared_;
  GatedB* b_;

 private:
  std::mutex mutex_;
};

class GatedB {
 public:
  void lock_b() { std::lock_guard<std::mutex> l(mutex_); }
  void FP_lock_shared_then_call_a_ok() {
    shared_->lock();
    {
      std::lock_guard<std::mutex> l(mutex_);
      a_->lock_a();
    }
    shared_->unlock();
  }

  Shared* shared_;
  GatedA* a_;

 private:
  std::mutex mutex_;
};

void GatedA::FP_lock_shared_then_call_b_ok() {
  shared_->lock();
  {
    std::lock_guard<std::mutex> l(mutex_);
    b_->lock_b();
  }
  shared_->unlock();
}

class MemberGateW;

// the gate that MemberGateW takes through g_ is assumed to be the gate of this
// object, as for parameters, so the deadlock between two distinct objects is
// missed
class MemberGateX {
 public:
  void FN_gate_then_ba_bad(MemberGateW* w);

  std::mutex gate_;
};

class MemberGateW {
 public:
  void gate_then_ab() {
    std::lock_guard<std::mutex> l1(g_->gate_);
    std::lock_guard<std::mutex> l2(a_);
    std::lock_guard<std::mutex> l3(b_);
  }

  MemberGateX* g_;
  std::mutex a_;
  std::mutex b_;
};

void MemberGateX::FN_gate_then_ba_bad(MemberGateW* w) {
  std::lock_guard<std::mutex> l1(gate_);
  std::lock_guard<std::mutex> l2(w->b_);
  std::lock_guard<std::mutex> l3(w->a_);
}

} // namespace member_locks
