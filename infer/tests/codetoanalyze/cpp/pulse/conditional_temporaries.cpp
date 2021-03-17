/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <iostream>

namespace condtemp {

// keep track of how many copies are made of the object and how many
// of these copies have been destroyed
struct Counter {
  std::string name;
  int copies;
  int copies_destroyed;
  bool destroyed;

  Counter(std::string name_)
      : name(name_), copies(0), copies_destroyed(0), destroyed(false) {}
};

struct X {
  int f; // some data
  Counter* counter; // nullptr if this object is a copy from another X
  Counter* original_counter; // nullptr if this object was created not from a
                             // copy or move constructor

  X() = delete;
  X(Counter* counter_) : counter(counter_), original_counter(nullptr), f(72) {
    //    std::cerr << "original: " << name() << "\n";
  }
  X(X& x) {
    std::cerr << "copy (of " << x.name()
              << ") constructor called with f=" << x.f << "\n";
    f = x.f + 11;
    copy_from(x);
  }
  X(X&& x) {
    std::cerr << "move (of " << x.name()
              << ") constructor called with f=" << x.f << "\n";
    f = x.f + 10;
    copy_from(x);
  }

  // should realise [this] cannot be null to avoid FP latent (that can never be
  // manifested)
  ~X() {
    std::cerr << "~X(" << f << ") " << name();
    if (original_counter) {
      original_counter->copies_destroyed++;
    }
    if (counter) {
      std::cerr << " original destroyed";
      counter->destroyed = true;
    }
    std::cerr << " \n";
    ;
  }

 private:
  void copy_from(X& x) {
    counter = nullptr;
    if (x.counter) {
      original_counter = x.counter;
    } else {
      original_counter = x.original_counter;
    }
    original_counter->copies++;
  }

  std::string& name() {
    if (counter) {
      return counter->name;
    } else {
      return original_counter->name;
    }
  }
};

X copy(X x) {
  std::cerr << "copy(" << x.f << ")\n";
  return x;
}

void crash(bool b) {
  if (b) {
  }
}

void FP_track_copy_operations_one_copy_ok() {
  Counter c_true("c_true"), c_false("c_false");
  X y(&c_false);
  X x = false ? X(&c_true) : y;
  std::cerr << "c_false.copies=" << c_false.copies << "\n";
  std::cerr << "c_false.copies_destroyed=" << c_false.copies_destroyed << "\n";
  std::cerr << "c_false.destroyed=" << c_false.destroyed << "\n";
  std::cerr << "c_true.copies=" << c_true.copies << "\n";
  std::cerr << "c_true.copies_destroyed=" << c_true.copies_destroyed << "\n";
  std::cerr << "c_true.destroyed=" << c_true.destroyed << "\n";
  // these values were checked against the output of the program
  // compiled with clang -fno-elide-constructors, results will vary
  // if we omit the elidable constructor calls
  if (!(c_false.copies == 2 && c_false.copies_destroyed == 1 &&
        !c_false.destroyed) ||
      c_true.copies != 0 || c_true.copies_destroyed != 0 || c_true.destroyed) {
    int* p = nullptr;
    *p = 42;
  }
}

void FP_track_copy_operations_complex_ok() {
  Counter c_true("c_true"), c_false("c_false");
  X y(&c_false);
  X x = true ? copy(X(&c_true)) : y;
  std::cerr << "c_false.copies=" << c_false.copies << "\n";
  std::cerr << "c_false.copies_destroyed=" << c_false.copies_destroyed << "\n";
  std::cerr << "c_false.destroyed=" << c_false.destroyed << "\n";
  std::cerr << "c_true.copies=" << c_true.copies << "\n";
  std::cerr << "c_true.copies_destroyed=" << c_true.copies_destroyed << "\n";
  std::cerr << "c_true.destroyed=" << c_true.destroyed << "\n";
  // these values were checked against the output of the program
  // compiled with clang -fno-elide-constructors, results will vary
  // if we omit the elidable constructor calls
  if (!(c_true.copies == 4 && c_true.copies_destroyed == 3 &&
        c_true.destroyed)) {
    int* p = nullptr;
    *p = 42;
  }
}

} // namespace condtemp

int main() { condtemp::FP_track_copy_operations_complex_ok(); }
