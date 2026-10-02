/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <atomic>
#include <deque>
#include <forward_list>
#include <functional>
#include <iterator>
#include <list>
#include <map>
#include <mutex>
#include <queue>
#include <set>
#include <shared_mutex>
#include <stack>
#include <string>
#include <unordered_map>
#include <vector>

namespace containers {

struct A {
  int value;
};

struct B {

  // operator[] of a map inserts missing keys, so FN_get_bad writes the map
  // without the lock, but RacerD does not report unprotected writes in C++
  void FN_write_container_bad(int key, int value) {
    mutex_.lock();
    map[key].value = value;
    mutex_.unlock();
  }

  int FN_get_bad(int key) { return map[key].value; }

  int size_bad() { return map.size(); }

 private:
  std::map<int, A> map;
  std::mutex mutex_;
};

struct Vector {
  void push_back(int x) {
    std::lock_guard<std::mutex> lock(mutex_);
    vec_.push_back(x);
  }

  size_t size_bad() { return vec_.size(); }

  int index_bad(size_t i) { return vec_[i]; }

  int iterate_bad() {
    int sum = 0;
    for (auto it = vec_.begin(); it != vec_.end(); ++it) {
      sum += *it;
    }
    return sum;
  }

  // range-for accesses the container through a compiler-generated local
  // reference, which is not tracked
  int FN_range_for_bad() {
    int sum = 0;
    for (int x : vec_) {
      sum += x;
    }
    return sum;
  }

  size_t size_locked_ok() {
    std::lock_guard<std::mutex> lock(mutex_);
    return vec_.size();
  }

 private:
  std::mutex mutex_;
  std::vector<int> vec_;
};

struct VectorElements {
  // the write goes through the reference returned by operator[], so only a
  // read of the container is recorded
  void set(size_t i, int x) {
    std::lock_guard<std::mutex> lock(mutex_);
    vec_[i] = x;
  }

  int FN_get_bad(size_t i) { return vec_[i]; }

 private:
  std::mutex mutex_;
  std::vector<int> vec_;
};

struct Map {
  void emplace(int key, int value) {
    std::lock_guard<std::mutex> lock(mutex_);
    map_.emplace(key, value);
  }

  void erase(int key) {
    std::lock_guard<std::mutex> lock(mutex_);
    map_.erase(key);
  }

  // the calls to find and end on one line give one report
  bool find_bad(int key) { return map_.find(key) != map_.end(); }

  // one report per line
  bool find_then_end_bad(int key) {
    auto it = map_.find(key);
    return it != map_.end();
  }

  size_t count_bad(int key) { return map_.count(key); }

  int at_bad(int key) { return map_.at(key); }

 private:
  std::mutex mutex_;
  std::map<int, int> map_;
};

struct UnorderedMap {
  // operator[] of a map inserts missing keys, so it is a write
  void put(int key, int value) {
    std::lock_guard<std::mutex> lock(mutex_);
    map_[key] = value;
  }

  bool find_bad(int key) { return map_.find(key) != map_.end(); }

  bool empty_bad() { return map_.empty(); }

 private:
  std::mutex mutex_;
  std::unordered_map<int, int> map_;
};

// non-const accessors do not modify the containers, so they do not race with
// the unprotected reads
struct NonConstAccessors {
  int locked_get_ok(int key) {
    std::lock_guard<std::mutex> lock(mutex_);
    auto it = map_.find(key);
    int found = it == map_.end() ? 0 : it->second;
    bool has_name = names_.find("name") != names_.end();
    bool flist_empty = std::next(flist_.before_begin()) == flist_.end();
    return found + map_.at(key) + vec_.front() + vec_[0] + *str_.data() +
           stack_.top() + has_name + flist_empty;
  }

  size_t unlocked_size_ok() {
    return map_.size() + names_.size() + vec_.size() + str_.size() +
           stack_.size() + flist_.empty();
  }

 private:
  std::mutex mutex_;
  std::map<int, int> map_;
  std::map<std::string, int, std::less<>> names_;
  std::vector<int> vec_;
  std::string str_;
  std::stack<int> stack_;
  std::forward_list<int> flist_;
};

// the container is only modified while the object is owned by its constructor
struct InitializedInConstructor {
  InitializedInConstructor() {
    std::lock_guard<std::mutex> lock(mutex_);
    vec_.push_back(1);
  }

  size_t size_ok() { return vec_.size(); }

 private:
  std::mutex mutex_;
  std::vector<int> vec_;
};

struct Queue {
  void push(int x) {
    std::lock_guard<std::mutex> lock(mutex_);
    queue_.push(x);
  }

  bool empty_bad() { return queue_.empty(); }

  int front_bad() { return queue_.front(); }

 private:
  std::mutex mutex_;
  std::queue<int> queue_;
};

struct DequeListSet {
  void add(int x) {
    std::lock_guard<std::mutex> lock(mutex_);
    deque_.push_front(x);
    list_.remove(x);
    set_.insert(x);
  }

  int deque_back_bad() { return deque_.back(); }

  size_t list_size_bad() { return list_.size(); }

  bool set_contains_bad(int x) { return set_.count(x) > 0; }

 private:
  std::mutex mutex_;
  std::deque<int> deque_;
  std::list<int> list_;
  std::set<int> set_;
};

struct String {
  void set(const std::string& s) {
    std::lock_guard<std::mutex> lock(mutex_);
    str_ = s;
  }

  size_t size_bad() { return str_.size(); }

  const char* c_str_bad() { return str_.c_str(); }

 private:
  std::mutex mutex_;
  std::string str_;
};

struct SharedMutexMap {
  void put(int key, int value) {
    std::unique_lock<std::shared_mutex> lock(mutex_);
    map_[key] = value;
  }

  bool contains_ok(int key) {
    std::shared_lock<std::shared_mutex> lock(mutex_);
    return map_.count(key) > 0;
  }

  bool empty_ok() {
    mutex_.lock_shared();
    bool empty = map_.empty();
    mutex_.unlock_shared();
    return empty;
  }

  size_t size_bad() { return map_.size(); }

  // operator[] inserts missing keys, so it races with the other readers, but
  // RacerD treats shared locks as exclusive
  int FN_get_or_insert_bad(int key) {
    std::shared_lock<std::shared_mutex> lock(mutex_);
    return map_[key];
  }

 private:
  std::shared_mutex mutex_;
  std::unordered_map<int, int> map_;
};

struct Splice {
  void move_all() {
    std::lock_guard<std::mutex> lock(mutex_);
    to_.splice(to_.end(), from_);
  }

  size_t to_size_bad() { return to_.size(); }

  // splice also empties its argument, but only the receiver is recorded as
  // written
  size_t FN_from_size_bad() { return from_.size(); }

 private:
  std::mutex mutex_;
  std::list<int> to_;
  std::list<int> from_;
};

struct LazyInit {
  // the atomic flag initialized_ orders the read of vec_ after the write, but
  // RacerD ignores the ordering that atomics provide
  size_t FP_size_ok() {
    if (!initialized_.load()) {
      std::lock_guard<std::mutex> lock(mutex_);
      if (!initialized_.load()) {
        vec_.push_back(1);
        initialized_.store(true);
      }
    }
    return vec_.size();
  }

 private:
  std::atomic<bool> initialized_{false};
  std::mutex mutex_;
  std::vector<int> vec_;
};

struct FreeFunctions {
  void push_back(int x) {
    std::lock_guard<std::mutex> lock(mutex_);
    vec_.push_back(x);
  }

  // only the member functions of the containers are modelled, not the free
  // functions std::size and std::empty
  size_t FN_size_bad() { return std::size(vec_); }

  bool FN_empty_bad() { return std::empty(vec_); }

 private:
  std::mutex mutex_;
  std::vector<int> vec_;
};

struct PrepopulatedMap {
  PrepopulatedMap() {
    for (int key = 0; key < 4; key++) {
      map_[key] = 0;
    }
  }

  // the key is always present, so operator[] only writes the mapped value
  void update(int key, int value) {
    std::lock_guard<std::mutex> lock(mutex_);
    map_[key & 3] = value;
  }

  // RacerD treats every operator[] on a map as an insertion
  bool FP_contains_ok(int key) { return map_.count(key) > 0; }

 private:
  std::mutex mutex_;
  std::map<int, int> map_;
};
} // namespace containers
