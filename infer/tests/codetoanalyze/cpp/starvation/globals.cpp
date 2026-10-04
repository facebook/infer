/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace globals {

std::mutex mutex_1;
std::mutex mutex_2;
int x;

// deadlock between global_1_then_2_bad() and global_2_then_1_bad()
void global_1_then_2_bad() {
  std::lock_guard<std::mutex> lock1(mutex_1);
  std::lock_guard<std::mutex> lock2(mutex_2);
  x++;
}

void global_2_then_1_bad() {
  mutex_2.lock();
  mutex_1.lock();
  x--;
  mutex_1.unlock();
  mutex_2.unlock();
}

std::mutex ordered_mutex_1;
std::mutex ordered_mutex_2;

// same order everywhere, no deadlock
void ordered_1_ok() {
  std::lock_guard<std::mutex> lock1(ordered_mutex_1);
  std::lock_guard<std::mutex> lock2(ordered_mutex_2);
  x++;
}

void ordered_2_ok() {
  std::lock_guard<std::mutex> lock1(ordered_mutex_1);
  std::lock_guard<std::mutex> lock2(ordered_mutex_2);
  x--;
}

static std::mutex file_mutex_1;
static std::mutex file_mutex_2;

class FileLocalMutexes {
 public:
  // deadlock between file_1_then_2_bad() and file_2_then_1_bad()
  void file_1_then_2_bad() {
    std::lock_guard<std::mutex> lock1(file_mutex_1);
    std::lock_guard<std::mutex> lock2(file_mutex_2);
    y_++;
  }

  void file_2_then_1_bad() {
    std::lock_guard<std::mutex> lock2(file_mutex_2);
    std::lock_guard<std::mutex> lock1(file_mutex_1);
    y_--;
  }

 private:
  int y_;
};

class StaticMembers {
 public:
  // deadlock between first_then_second_bad() and second_then_first_bad()
  void first_then_second_bad() {
    std::lock_guard<std::mutex> lock1(first_);
    std::lock_guard<std::mutex> lock2(second_);
    y_++;
  }

  void second_then_first_bad() {
    std::lock_guard<std::mutex> lock2(second_);
    std::lock_guard<std::mutex> lock1(first_);
    y_--;
  }

 private:
  static std::mutex first_;
  static std::mutex second_;
  int y_;
};

std::mutex StaticMembers::first_;
std::mutex StaticMembers::second_;

std::mutex global_mutex;

class MemberAndGlobal {
 public:
  // deadlock between member_then_global_bad() and global_then_member_bad()
  void member_then_global_bad() {
    std::lock_guard<std::mutex> lock1(mutex_);
    std::lock_guard<std::mutex> lock2(global_mutex);
    y_++;
  }

  void global_then_member_bad() {
    std::lock_guard<std::mutex> lock2(global_mutex);
    std::lock_guard<std::mutex> lock1(mutex_);
    y_--;
  }

 private:
  std::mutex mutex_;
  int y_;
};

struct WithMutexes {
  std::mutex mutex_1;
  std::mutex mutex_2;
  int y;

  // deadlock with free_function_2_then_1_bad()
  void method_1_then_2_bad() {
    std::lock_guard<std::mutex> lock1(mutex_1);
    std::lock_guard<std::mutex> lock2(mutex_2);
    y++;
  }
};

void free_function_2_then_1_bad(WithMutexes* s) {
  std::lock_guard<std::mutex> lock2(s->mutex_2);
  std::lock_guard<std::mutex> lock1(s->mutex_1);
  s->y--;
}

std::mutex self_mutex;

void self_deadlock_bad() {
  self_mutex.lock();
  self_mutex.lock();
  x++;
  self_mutex.unlock();
  self_mutex.unlock();
}

void lock_self_mutex() {
  std::lock_guard<std::mutex> lock(self_mutex);
  x++;
}

void interproc_self_deadlock_bad() {
  std::lock_guard<std::mutex> lock(self_mutex);
  lock_self_mutex();
}

std::recursive_mutex global_recursive_mutex;

void lock_recursive_mutex() {
  std::lock_guard<std::recursive_mutex> lock(global_recursive_mutex);
  x++;
}

void recursive_relock_ok() {
  std::lock_guard<std::recursive_mutex> lock(global_recursive_mutex);
  lock_recursive_mutex();
}

void lock_and_increment(std::mutex& mutex) {
  std::lock_guard<std::mutex> lock(mutex);
  x++;
}

std::mutex helper_mutex_1;
std::mutex helper_mutex_2;

// deadlock between helper_1_then_2_bad() and helper_2_then_1_bad(), where the
// helper takes the global passed to it
void helper_1_then_2_bad() {
  std::lock_guard<std::mutex> lock1(helper_mutex_1);
  lock_and_increment(helper_mutex_2);
}

void helper_2_then_1_bad() {
  std::lock_guard<std::mutex> lock2(helper_mutex_2);
  lock_and_increment(helper_mutex_1);
}

void helper_self_deadlock_bad() {
  std::lock_guard<std::mutex> lock(self_mutex);
  lock_and_increment(self_mutex);
}

void add_under_lock(int i, std::mutex& mutex) {
  std::lock_guard<std::mutex> lock(mutex);
  x += i;
}

std::mutex mutex_for_parameter;

// deadlock between global_then_parameter_bad() and
// parameter_then_global_bad(), where the helper takes its second argument
void global_then_parameter_bad(std::mutex& mutex) {
  std::lock_guard<std::mutex> lock1(mutex_for_parameter);
  add_under_lock(1, mutex);
}

void parameter_then_global_bad(std::mutex& mutex) {
  std::lock_guard<std::mutex> lock2(mutex);
  std::lock_guard<std::mutex> lock1(mutex_for_parameter);
  x--;
}

std::mutex init_mutex_1;
std::mutex init_mutex_2;

// deadlock with init_2_then_1_bad(), not reported on the initializer of
// initialized_global, which runs before any thread is started
int init_1_then_2_bad() {
  std::lock_guard<std::mutex> lock1(init_mutex_1);
  std::lock_guard<std::mutex> lock2(init_mutex_2);
  return 1;
}

int initialized_global = init_1_then_2_bad();

void init_2_then_1_bad() {
  std::lock_guard<std::mutex> lock2(init_mutex_2);
  std::lock_guard<std::mutex> lock1(init_mutex_1);
  x++;
}

std::mutex init_mutex_3;
std::mutex init_mutex_4;

int init_3_then_4_ok = (init_mutex_3.lock(),
                        init_mutex_4.lock(),
                        init_mutex_4.unlock(),
                        init_mutex_3.unlock(),
                        1);

// the initializer of init_3_then_4_ok takes the locks in the other order, but
// runs before any thread is started
void init_4_then_3_ok() {
  std::lock_guard<std::mutex> lock4(init_mutex_4);
  std::lock_guard<std::mutex> lock3(init_mutex_3);
  x++;
}

std::mutex init_self_mutex;

// a self deadlock does not need another thread
int init_self_deadlock_bad = (init_self_mutex.lock(),
                              init_self_mutex.lock(),
                              init_self_mutex.unlock(),
                              init_self_mutex.unlock(),
                              1);

} // namespace globals
