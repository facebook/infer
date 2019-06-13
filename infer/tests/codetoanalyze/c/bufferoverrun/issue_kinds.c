/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int zero_or_ten(int ten) {
  if (ten) {
    return 10;
  } else {
    return 0;
  }
}

int zero_to_infty() {
  int r = 0;
  for (int i = 0; i < zero_or_ten(0); i++) {
    r++;
  }
  return r;
}

void l1_concrete_overrun_Bad() {
  int a[10];
  a[10] = 0;
}

void l1_concrete_underrun_Bad() {
  int a[10];
  a[-1] = 0;
}

void l1_symbolic_overrun_Bad(int i) {
  int a[10];
  if (i >= 10) {
    a[i] = 0;
  }
}

void l1_symbolic_overrun2_Bad(int n) {
  int a[n];
  a[n] = 0;
}

void l1_symbolic_underrun_Bad(int i) {
  int a[10];
  if (i < 0) {
    a[i] = 0;
  }
}

int less_than(int i, int n) { return i < n; }

void l1_symbolic_widened_Bad(int n) {
  int a[n];
  for (int i = n; less_than(i, 2 * n); i++) {
    a[i] = 0;
  }
}

void l1_symbolic_widened_Good(int n) {
  int a[n];
  for (int i = n; less_than(i, n); i++) {
    a[i] = 0;
  }
}

void l2_concrete_overrun_Bad() {
  int a[10];
  a[zero_or_ten(1)] = 0;
}

void l2_concrete_underrun_Bad() {
  int a[9];
  a[zero_or_ten(0) - 1] = 0;
}

void l2_concrete_no_overrun_Good_FP() {
  int a[10];
  a[zero_or_ten(0)] = 0;
}

void l2_concrete_no_underrun_Good_FP() {
  int a[9];
  a[zero_or_ten(1) - 1] = 0;
}

void l2_symbolic_overrun_Bad(int* n) {
  int a[*n];
  a[*n] = 0;
}

void l2_symbolic_no_overrun_Good(int n) {
  int a[n];
  if (n > 0) {
    a[n - 1] = 0;
  }
}

void l3_concrete_overrun_Bad() {
  int a[zero_or_ten(0) + 5];
  a[zero_or_ten(1)] = 0;
}

void l3_concrete_underrun_Bad() {
  int a[10];
  a[zero_or_ten(0) - 1] = 0;
}

void l3_concrete_no_overrun_Good_FP() {
  int a[zero_or_ten(1) + 5];
  a[zero_or_ten(1)] = 0;
}

void l3_concrete_no_underrun_Good_FP() {
  int a[10];
  a[zero_or_ten(1) - 1] = 0;
}

void l4_widened_overrun_Bad() {
  int a[10];
  for (int i = 0; less_than(i, 11); i++) {
    a[i] = 0;
  }
}

void l4_widened_no_overrun_Good_FP() {
  int a[10];
  for (int i = 0; less_than(i, 10); i++) {
    a[i] = 0;
  }
}

int unknown_function();

void l5_external_Warn_Bad() {
  int a[10];
  a[unknown_function()] = 0;
}

void s2_symbolic_widened_Bad(int* n) {
  int a[*n];
  for (int i = *n; less_than(i, 2 * *n); i++) {
    a[i] = 0;
  }
}

void s2_symbolic_widened_Good_FP(int* n) {
  int a[*n];
  for (int i = *n; less_than(i, *n); i++) {
    a[i] = 0;
  }
}

// Do not report as it was already reported in the callee with the same issue
// type
void FP_call_s2_symbolic_widened_Silenced(int* m) {
  s2_symbolic_widened_Bad(m);
}

void l1_call_to_s2_symbolic_widened_Bad() {
  int x = 1;
  s2_symbolic_widened_Bad(&x);
}

void may_underrun_symbolic_Nowarn_Good(int n) {
  int a[n];
  a[n - 1] = 0;
}

void may_over_or_underrun_symbolic_Nowarn_Good(int n) {
  int a[10];
  a[n] = 0;
}

void may_over_or_underrun_symbolic2_Nowarn_Good(int n) {
  int a[n];
  a[1] = 0;
}

void alloc_is_negative_Bad() { malloc(-2); }

void alloc_may_be_negative_Bad() { malloc(zero_or_ten(0) - 5); }

void alloc_may_be_negative_Good_FP() { malloc(zero_or_ten(1) - 5); }

void alloc_is_zero_Bad() { malloc(0 * sizeof(int)); }

void alloc_is_big_Bad() { malloc(2 * 1000 * 1000 * 1000); }

void alloc_may_be_big_Bad() { malloc(zero_or_ten(1) * 100 * 1000 * 1000 + 1); }

// Non-symbolic, should not be propagated
void call_to_alloc_may_be_big_Good() { alloc_may_be_big_Bad(); }

void alloc_may_be_big_Good_FP() {
  malloc(zero_or_ten(1) * 100 * 1000 * 1000 + 1);
}

/*
  When the upper bound is infinity and the lower bound unknown,
  we don't report but still propagate the error.
*/
void alloc_may_be_big2_Silenced(int n) { malloc(n + zero_to_infty()); }

// Now that we have a lower bound, we can report it
void call_to_alloc_may_be_big2_is_big_Bad() {
  alloc_may_be_big2_Silenced(100 * 1000 * 1000);
}

void l1_unknown_function_Bad() {
  int a[5];
  int idx = unknown_function() * 10;
  if (10 <= idx) {
    if (idx <= 10) {
      a[idx] = 0;
    }
  }
}

/*
  We do not report the underrun here, in case the loop never runs (length <= 0).
  But we should report it anyway.
*/
void loop_underrun_Bad_FN(int length) {
  int i;
  char a[length];

  for (i = length - 1; i >= 0; i--) {
    a[i - 1] = 'U';
  }
}

void l2_loop_overflow_Bad(int length) {
  int i;
  char a[length];

  for (i = length - 1; i >= 0; i--) {
    a[i + 1] = 'O';
  }
}

void l2_loop_overflow2_Bad(int length) {
  int i;
  char a[length];

  for (i = length - 1; i >= 0; i--) {
    a[length - i] = 'O';
  }
}

/* Inferbo raises U5 alarm because
   - the pair of offset:[10,10] and size:[5,+oo] is belong to L3
   - the offset value is from an unknown function
   - there is at least one infinity bound (in size).
   However, it should ideally raise L3, because the infinity is not
   from the unknown function. */
void False_Issue_Type_l3_unknown_function_Bad() {
  int* a = (int*)malloc((zero_to_infty() + 5) * sizeof(int));
  int idx = unknown_function() * 10;
  if (10 <= idx) {
    if (idx <= 10) {
      a[idx] = 0;
    }
  }
}

int mone_to_one() {
  int x = unknown_function();
  if (x >= -1 && x <= 1) {
    return x;
  } else {
    return 0;
  }
}

void two_safety_conditions(int n) {
  char a[10];
  int y = mone_to_one();
  if (unknown_function()) {
    a[n] = 0; // should be L1 when n=10
  } else {
    a[n + y] = 0; // should be L2 when n=10
  }
}

void call_two_safety_conditions_l1_and_l2_Bad() { two_safety_conditions(10); }

/* issue1 and issue2 are deduplicated since the offset of issue2
   ([10,+oo]) subsumes that of issue1 ([10,10]). */
void deduplicate_issues_1_Bad() {
  int a[10];
  int x = 10;
  a[x] = 0; // issue1: [10,10] < [10,10]
  x = unknown_function();
  if (x >= 10) {
    a[x] = 0; // issue2: [10,+oo] < [10,10]
  }
}

/* issue1 and issue2 are not deduplicated since they have different
   conditions to reach. */
void deduplicate_issues_2_Bad(int y) {
  int a[10];
  int x = 10;
  a[x] = 0; // issue1: [10,10] < [10,10]
  x = unknown_function();
  if (x >= 10 && y >= 0) {
    a[x] = 0; // issue2: [10,+oo] < [10,10]
  }
}
