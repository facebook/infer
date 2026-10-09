/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct CorrelatedQuery {
  int stats_;
  // Intended contract for this fixture: open() does not read stats_. The
  // implementation is unavailable, so Pulse cannot infer that read footprint.
  bool open() const;
  void touch();
};

// FP: invalidation uses the whole reachable object, so a statistics-only write
// decorrelates the two queries even though their intended results agree.
void FP_correlated_after_field_write_ok(CorrelatedQuery& c) {
  int x = 0;
  int* p = nullptr;
  if (c.open()) {
    p = &x;
  }
  c.stats_++;
  if (c.open()) {
    *p = 42;
  }
}

// A non-const unknown method may change open()'s result: this is a true bug.
void correlated_after_unknown_mutation_bad(CorrelatedQuery& c) {
  int x = 0;
  int* p = nullptr;
  if (c.open()) {
    p = &x;
  }
  c.touch();
  if (c.open()) {
    *p = 42;
  }
}
