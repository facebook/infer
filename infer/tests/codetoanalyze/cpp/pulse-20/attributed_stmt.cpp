/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace attributed_stmt {

struct S {
  int x;
};

int deref(S* p) { return p->x; }

int npe_before_unlikely_branch_bad(int n) {
  S* p = nullptr;
  int x = p->x;
  if (n < 0) [[unlikely]] {
    return -1;
  }
  return x;
}

int npe_in_likely_branch_bad() {
  S* p = nullptr;
  int n = 1;
  if (n > 0) [[likely]] {
    return p->x;
  }
  return 0;
}

int npe_in_likely_case_bad() {
  S* p = nullptr;
  int n = 0;
  switch (n) {
    [[likely]] case 0:
      return p->x;
    default:
      return 0;
  }
}

int npe_after_pragma_unroll_loop_bad() {
  S* p = nullptr;
  int sum = 0;
#pragma unroll
  for (int i = 0; i < 2; i++) {
    sum += i;
  }
  return sum + p->x;
}

// [[clang::musttail]] requires the caller to have the same signature as the
// callee
int npe_musttail_bad(S* q) { [[clang::musttail]] return deref(nullptr); }

int deref_after_nomerge_call(S* p) {
  [[clang::nomerge]] deref(p);
  return p->x;
}

int npe_call_function_with_attributed_stmt_bad() {
  return deref_after_nomerge_call(nullptr);
}

// `(void)c;` and `c;` create no CFG node of their own

static void void_likely_branch(int c) {
  if (c) [[likely]]
    (void)c;
}

int npe_after_void_likely_branch_call_bad() {
  S* p = nullptr;
  void_likely_branch(1);
  return p->x;
}

static void likely_branch_and_void_branch(int c) {
  if (c > 1) [[likely]] {
    c--;
  }
  if (c)
    (void)c;
}

int npe_after_likely_branch_and_void_branch_call_bad() {
  S* p = nullptr;
  likely_branch_and_void_branch(1);
  return p->x;
}

int npe_after_likely_nodeless_label_bad(int c) {
  S* p = nullptr;
  goto out;
out:
  [[likely]] c;
  return p->x;
}

int npe_after_unlikely_nodeless_default_bad() {
  S* p = nullptr;
  int k = 2;
  switch (k) {
    case 1:
      break;
    default:
      [[unlikely]] k;
  }
  return p->x;
}

int likely_null_check_ok(S* p) {
  if (p != nullptr) [[likely]] {
    return p->x;
  }
  return 0;
}

int assume_is_not_evaluated_ok() {
  S* p = nullptr;
  [[assume(p->x > 0)]];
  return 0;
}

} // namespace attributed_stmt
