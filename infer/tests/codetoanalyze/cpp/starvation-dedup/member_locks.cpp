/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace member_locks {

class Owner {
 public:
  // the deadlocks of the clients with this method are only found from the
  // clients, as the paths of these locks do not go through the client classes,
  // and are reported whether the name of the client class comes before or
  // after this one
  void lock_first_then_second() {
    std::lock_guard<std::mutex> l1(first_);
    std::lock_guard<std::mutex> l2(second_);
  }

  std::mutex first_;
  std::mutex second_;
};

class AClient {
 public:
  void lock_second_then_first_bad() {
    std::lock_guard<std::mutex> l1(owner_->second_);
    std::lock_guard<std::mutex> l2(owner_->first_);
  }

  Owner* owner_;
};

class ZClient {
 public:
  void lock_second_then_first_bad() {
    std::lock_guard<std::mutex> l1(owner_->second_);
    std::lock_guard<std::mutex> l2(owner_->first_);
  }

  Owner* owner_;
};

class MemberB;

// the deadlock between lock_then_call_b_bad() and
// MemberB::lock_then_call_a_bad() is found from both sides but reported once
class MemberA {
 public:
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

std::mutex global_mutex;

class MultiOther;

// the deadlock between global_and_self_then_other_bad() and
// MultiOther::self_then_global_and_client_bad() is found from both sides, from
// MultiOther through the second lock that std::lock acquires, but reported once
class MultiClient {
 public:
  void global_and_self_then_other_bad();

  MultiOther* other_;
  std::mutex mutex_;
};

class MultiOther {
 public:
  void self_then_global_and_client_bad() {
    std::lock_guard<std::mutex> l(mutex_);
    std::lock(global_mutex, client_->mutex_);
    global_mutex.unlock();
    client_->mutex_.unlock();
  }

  MultiClient* client_;
  std::mutex mutex_;
};

void MultiClient::global_and_self_then_other_bad() {
  std::lock_guard<std::mutex> l1(global_mutex);
  std::lock_guard<std::mutex> l2(mutex_);
  std::lock_guard<std::mutex> l3(other_->mutex_);
}

} // namespace member_locks
