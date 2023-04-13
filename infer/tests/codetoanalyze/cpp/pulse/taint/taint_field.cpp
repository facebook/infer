/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef struct {
  int manipulated;
  int other;
} structure;

int int_source(void);
void taint_structure(structure);
void taint_manipulated(structure);
void taint_manipulated_with_indirections(structure**);

void sink_int(int);
void sink_manipulated(structure);

void sanitize_manipulated(structure);

void propagate_to_manipulated(structure, int);

void test_taint_field_bad(structure s) {
  taint_manipulated(s);
  sink_int(s.manipulated);
}

void test_taint_field_good(structure s) {
  taint_manipulated(s);
  sink_int(s.other);
}

void test_sink_field_bad(structure s) {
  int tainted = int_source();
  s.manipulated = tainted;
  sink_manipulated(s);
}

void test_sink_field_good(structure s) {
  int tainted = int_source();
  s.other = tainted;
  sink_manipulated(s);
}

void test_sanitize_field_bad(structure s) {
  // The 2 lines below are necessary because tainting propagates down
  // what is known in the memory at the moment of tainting and it is never
  // propagated again when a new manipulated appears. `s.other` and
  // `s.manipulated` need to have different values otherwise they are both
  // referencing the same value and sanitizing one would sanitize the other
  s.other = 2; // makes s.other exist in memory before tainting s
  s.manipulated = 1; // makes s.manipulated exist in memory before tainting s
  taint_structure(s);
  sanitize_manipulated(s);
  sink_int(s.other);
}

void test_sanitize_field_good(structure s) {
  s.other = 0; // makes s.other exist in memory before tainting s
  s.manipulated = 1; // makes s.manipulated exist in memory before tainting s
  taint_structure(s);
  sanitize_manipulated(s);
  sink_manipulated(s);
}
void test_propagate_to_field_bad(structure s) {
  int tainted = int_source();
  propagate_to_manipulated(s, tainted);
  sink_int(s.manipulated);
}

void test_propagate_to_field_good(structure s) {
  int tainted = int_source();
  propagate_to_manipulated(s, tainted);
  sink_int(s.other);
}

void test_taint_field_with_indirections_bad(structure s) {
  structure* s_ptr = &s;
  structure** s_ptr_ptr = &s_ptr;
  taint_manipulated_with_indirections(s_ptr_ptr);
  sink_int((**s_ptr_ptr).manipulated);
}

void test_taint_field_with_indirections_good(structure s) {
  structure* s_ptr = &s;
  structure** s_ptr_ptr = &s_ptr;
  taint_manipulated_with_indirections(s_ptr_ptr);
  sink_int((**s_ptr_ptr).other);
}
