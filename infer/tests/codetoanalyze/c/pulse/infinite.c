/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// works - OK
void empty_function_ok() { return; }

// works - OK
void one_liner_ok(int x) { x++; }

// works - OK
void two_liner_ok(int x) {
  x++;
  return;
}

void simple_goto_bad(int y) {
re:
  y++;
  goto re;
}

/* pulse-inf: works -- empty path condition, no bug */
void simple_loop0_ok() {
  int y = 0;
  while (y < 100)
    y++;
}

void FN_simple_loop0_bad() {
  int y = 0;
  int x = 0;
  while (y < 100)
    x++;
}

// pulse-inf ok no loop
void simple_goto_ok(int y) {
  y++;
  goto end;
end:
  return;
}

void FN_entry_point_calling_bad() {
  FN_simple_loop0_bad();
}

/* pulse-inf: Able to flag bug */
void conditional_goto0_bad(int y) {
re:
  if (y == 100)
    goto re;
  else
    return;
}

void conditional_goto0_ok(int y) {
re:
  if (y == 100) {
    y++;
    goto re;
  } else
    return;
}

/* pulse-inf: works good, find bug */
void fcall(int y) { y++; }

void loop_call_bad(int y) {
  while (y == 100)
    fcall(y);
  return;
}

void fcall_by_ref(int* y) { (*y) = (*y) + 1; }

void loop_call_ok(int y) {
  while (y == 100)
    fcall_by_ref(&y);
  return;
}

void twovars_goto_bad(int y) {
  int z = y;
  int x = 0;
label:
  x = 42;
  goto label;
}

/* pulse-inf: works good */
void loop_pointer_ok(int* x, int y) {
  int* z = x;
  // int y = 1;
  if (x != &y)
    while (y < 100) {
      y++;
      (*z)--;
    }
}

/* pulse-inf: works good */
void FN_loop_pointer_bad(int* x, int y) {
  int* z = x;
  // int y = 1;
  if (x == &y)
    while (y < 100) {
      y++;
      (*z)--;
    }
}

/* pulse-inf: works good */
void var_goto_ok(int y) {
  int x = 42;
  goto end;
  x++;
end:
  return;
}

/* pulse-inf: works good */
void loop_conditional_bad(int y) {
  int x = 0;
  while (y < 100)
    if (y < 50)
      x++;
    else
      y++;
}

void loop_conditional_ok(int y) {
  int x = 0;
  while (y < 100)
    if (y < 50) {
      x++;
      y = 50;
    } else
      y++;
}

/* pulse inf used to detect this! NEW FN */
void FN_nested_loop_cond_bad(int y) {
  int x = 42;
  while (y < 100) {
    while (x <= 100) {
      if (x == 50)
        x = 1;
      else
        x++;
    }
    y++;
  }
}

void FN_nested_external_bad(int y) {
  int x = 0;
  while (y < 100) {
    while (x < 3) {
      x++;
    }
  }
}

void nested_external_ok(int y) {
  int x = 0;
  while (y < 100) {
    while (x < 2) {
      x++;
    }
    y++;
  }
}

void FN_two_nested_fst_bad(int k) {
  int r = 0;
  for (int i = 0; i < k; r++)
    for (int j = 0; j < k; j++)
      r++;
  return r;
}

void FN_two_nested_snd_bad(int k) {
  int r = 0;
  for (int i = 0; i < k; i++)
    for (int j = 0; j < k; r++)
      r++;
  return r;
}

void two_nested_ok(int k) {
  int r = 0;
  for (int i = 0; i < k; i++)
    for (int j = 0; j < k; j++)
      r++;
  return r;
}

void nested_loop_cond_ok(int y) {
  int x = 42;
  while (y < 100) {
    while (x <= 100) {
      if (x == 50)
        x = 60;
      else
        x++;
    }
    y++;
  }
}

/* pulse inf works */
void simple_loop_bad(int x) {
  int y = 1;
  while (x != 3)
    y++;
}

void simple_loop_ok(int x) {
  int y = 1;
  while (y != 3)
    y++;
}

void FN_loop_alternating_bad(int y, int x) {
  int turn = 0;
  while (x < 100) {
    if (turn)
      x++;
    else
      x--;
    turn = (turn ? 0 : 1);
  }
}

void loop_alternating_ok(int y, int x) {
  int turn = 0;
  while (x < 100) {
    if (turn)
      x++;
    else
      x = x + 2;
    turn = (turn ? 0 : 1);
  }
}

void inner_loop_bad(int y, int x) {
  while (y < 100) {
    while (x == 0)
      y++;
    y++;
  }
}

void inner_loop_ok(int y, int x) {
  while (y < 100) {
    while (x == 0) {
      y++;
      x++;
    }
    y++;
  }
}

void simple_dowhile_ok(int y, int x) {
  do {
    y++;
    x++;
  } while (0);
}

void simple_dowhile_bad(int y, int x) {
  do {
    x++;
  } while (y > 0);
}

int conditional_goto_ok(int x, int y) {
re:
  x++;
  if (0) {
    int z1 = x * 2;
    goto re;
    return (z1);
  } else {
    int z2 = x + y;
    return z2;
  }
}

int conditional_goto_bad(int x, int y) {
re:
  x++;
  if (y) {
    int z1 = x * 2;
    goto re;
    return (z1);
  } else {
    int z2 = x + y;
    return z2;
  }
}

/* pulse-inf: FP due to Pulse computing arithmetic on Q rather than BV */
void FP_loop_signedarith_ok(int y) {
  while (y > 0x7fffffff) {
    y++;
    y--;
  }
  return;
}

/* pulse-inf: FP due to Pulse computing arithmetic on Q rather than BV */
void FP_goto_signedarith_ok(int y) {
re:
  if (y > 0x7fffffffffffffff)
    goto re;
  else
    return;
}

/* pulse-inf: works good! no bug */
void loop_with_break_ok(int y) {
  y = 0;
  while (y < 100)
    if (y == 50)
      break;
    else
      y++;
}

/* pulse-inf: works good! no bug */
void loop_with_break_var1_ok(int y) {
  y = 0;
  while (y < 100)
    if (y == 50) {
      y--;
      break;
    } else
      y++;
}

/* pulse-inf: works good! no bug */
void loop_with_break_var2_ok(int y) {
  while (y < 100)
    if (y == 50) {
      y--;
      break;
    } else
      y++;
}

/* pulse-inf: works! no bug */
void loop_with_break_var3_ok(int y) {
  while (y < 100)
    if (y == 50)
      break;
    else
      y++;
}

/* pulse-inf: works! no bug */
void loop_with_return_ok(int y) {
  while (y < 100)
    if (y == 50) {
      y--;
      return;
    } else
      y++;
}

/* pulse-inf: works! no bug */
void loop_with_return_ok_var1(int y) {
  while (y < 100)
    if (y == 50)
      return;
    else
      y++;
}

/* pulse-inf: works good! no bug */
void loop_with_return_var2_ok(int y) {
  y = 0;
  while (y < 100)
    if (y == 50) {
      y--;
      return;
    } else
      y++;
}

/* pulse-inf: works good! no bug */
void loop_with_return_var3_ok(int y) {
  y = 0;
  while (y < 100)
    if (y == 50)
      return;
    else
      y++;
}

void FN_while_ge_bad() {
  int i = 0;
  while ((i >= 0) == 1)
    i++;
}

void FN_while_even_bad() {
  int i = 0;
  while (i % 2 == 0)
    i = i + 2;
}

int incr_if_non_zero(int x, int y) {
  if (y == 0)
    return x;
  else
    return x + 1;
}

void incr_with_call_bad(int y) {
  int i = 0;
  while (i < 100)
    i = incr_if_non_zero(i, y);
}

int incr_if_geq_zero(int x) {
  if (x < 0)
    return x;
  else
    return x + 1;
}

void FP_incr_with_call_ok() {
  int i = 0;
  while (i < 100) {
    // FP because of abstraction: i could be < 0
    i = incr_if_geq_zero(i);
  }
}

int loop_repeated_ok(int i) {
  int val = 0;
  for (i = 0; i < 3; i++)
    val++;
  for (i = 0; i < 3; i++)
    val++;
  return (val);
}

/* pulse-inf: False negative -- maybe augment the numiters in pulseinf config */
// From: Gupta POPL 2008 - "Proving non-termination"
int FN_bsearch_gupta08_bad(int a[], int k, unsigned int lo, unsigned int hi) {
  unsigned int mid;

  while (lo < hi) {
    mid = (lo + hi) / 2;
    if (a[mid] < k) {
      lo = mid + 1;
    } else if (a[mid] > k) {
      hi = mid - 1;
    } else {
      return mid;
    }
  }
  return -1;
}

/* pulseinf: works fine - no bug detected */
// Cook et al. 2006 - TERMINATOR fails to prove termination
typedef struct s_list {
  int value;
  struct s_list* next;
} list_t;

/* pulse-inf: works good, no bug */
static void list_iter_cook06_ok(list_t* p) {
  int tot = 0;
  do {
    tot += p->value;
    p = p->next;
  } while (p != 0);
}

/* pulse-inf: works good, no bug */
static void list_iter_cook06_variant_ok(list_t* p) {
  int tot = 0;
  while (p != 0) {
    tot += p->value;
    p = p->next;
  }
}

/* pulse-inf: works good - no bug */
static void list_iter_cook06_variant2_ok(list_t* p) {
  int tot;
  for (tot = 0; p != 0; p = p->next) {
    tot += p->value;
  }
}

/* pulse-inf: works good - no bug */
// Cook et al. 2006 - TERMINATOR proves termination
void two_ints_loop_cook06_ok(int x, int y) {
  if (y >= 1)
    while (x >= 0)
      x = x + y;
}

/* Cook et al. 2006 - Prove termination with non-determinism involved */
int Ack(int x, int y) {
  if (x > 0) {
    int n;
    if (y > 0) {
      y--;
      n = Ack(x, y);
    } else {
      n = 1;
    }
    x--;
    return Ack(x, n);
  } else {
    return y + 1;
  }
}

#include <stdlib.h>

/* pulse-inf: works good! no bug */
int nondet() { return (rand()); }
int benchmark_nondet_cook06_ok() {
  int x = nondet();
  int y = nondet();

  int* p = &y;
  int* q = &x;
  int b = 1;

  while (x < 100 && 100 < y && b) {
    if (p == q) {
      int k = Ack(nondet(), nondet());
      (*p)++;
      while ((k--) > 100) {
        if (nondet()) {
          p = &y;
        }
        if (nondet()) {
          p = &x;
        }
        if (!b) {
          k++;
        }
      }
    } else {
      (*q)--;
      (*p)--;
      if (nondet()) {
        p = &y;
      }
      if (nondet()) {
        p = &x;
      }
    }
    b = nondet();
  }
  return (0);
}

/* pulseinf: works good - no bug detected */
// Cook et al. 2006 - termination with non determinism
// #include <stdlib.h>
// int nondet() { return (rand()); }
int npc = 0;
int nx, ny, nz;
void benchmark_cook06_ok() {
  int x = nondet(), y = nondet(), z = nondet();
  if (y > 0) {
    do {
      if (npc == 5) {
        if (!((y < z && z <= nz) || (x < y && x >= nx) || 0))
          ;
      }
      if (npc == 0) {
        if (nondet()) {
          nx = x;
          ny = y;
          nz = z;
          npc = 5;
        }
      }
      if (nondet()) {
        x = x + y;
      } else {
        z = x - y;
      }
    } while (x < y && y < z);
  }
}

/* pulseinf: works good - no bug */
// Cook et al. 2006 proves termination with non determinism
// #include <stdlib.h>
// int nondet() { return (rand()); }
void benchmark_simple_cook06_ok() {
  int x = nondet(), y = nondet(), z = nondet();
  if (y > 0) {
    do {
      if (nondet()) {
        x = x + y;
      } else {
        z = x - y;
      }
    } while (x < y && y < z);
  }
}

/* Simple non-det benchmark for non-terminate */
/* Inspired by cook'06 by flipping existing test benchmark_simple_ok_cook06 */
/* pulse-inf: works good! flag the bug */
void FN_nondet_loop_bad(int z) {
  int x = 1;
  while (x < z)
    if (nondet())
      x++;
}

/* From: AProVE: Non-termination proving for C Programs (Hensel et al. TACAS
 * 2022)*/
/* pulse-inf: Works good! (flag bug at regular widening threshold) */
void hensel_tacas22_bad(int x, int y) {
  y = 0;
  while (x > 0) {
    x--;
    y++;
  }
  while (y > 1)
    y = y;
}

/* Harris et al. "Alternation for Termination (SAS 2010) - Terminating program
 */
void foo(int* x) { (*x)--; }

/* Derived from Harris'10 */
/* Pulseinf: no bug */
void interproc_terminating_harris10_ok(int x) {
  while (x > 0)
    foo(&x);
}

/* Derived from Harris'10 */
/* Pulseinf: no bug */
void interproc_terminating_harris10_cond_ok(int x) {
  while (x > 0) {
    if (nondet())
      foo(&x);
    else
      foo(&x);
  }
}

/* Harris et al. "Alternation for Termination (SAS 2010) - Non Terminating
 * program */
/* TERMINATOR unable to find bug */
/* TREX find bug in 5sec */
/* pulse-inf: works good! Detect the bug! */
void FN_loop_non_terminating_harris10_bad(int x, int d, int z) {
  d = 0;
  z = 0;
  while (x > 0) {
    z++;
    x = x - d;
  }
}

/*** Chen et al. TACAS 2014 */
// TNT proves non-termination with non determinism
/* Pulse-inf: works good (also flag the bug) */
/* TO me: there is no bug here! problem in chen14 paper - the nondet() should
 * eventually make it break */
// #include <stdlib.h>
// int nondet() { return (rand()); }
void FN_nondet_nonterminate_chen14_bad(int k, int i) {
  if (k >= 0)
    ;
  else
    i = -1;
  while (i >= 0)
    i = nondet();
  i = 2;
}

/* TNT fails to prove non-termination
   If we assume, bounded arithmetic, this will terminate because
   k >= 0 will eventually be false due to integer wrap.
   But Pulse assumes unbounded arithmetic so this should be
   marked as an infinite recursion */
void FN_nestedloop2_chen14_bad(int k, int j) {
  while (k >= 0) {
    k++;
    j = k;
    while (j >= 1)
      j--;
  }
}

/* pulse-inf used to find bug - Now FN */
// TNT proves non-termination
void nestedloop_chen14_bad(int i) {
  if (i == 10) {
    while (i > 0) {
      i = i - 1;
      while (i == 0)
        ;
    }
  }
}

/****** Tests that reflect present in cryptographic libraries ********/
// Example with array - no manifest bug
void array_iter_ok(int array[]) {
  unsigned int i = 0;
  while (array[i] != 0) {
    array[i] = 42;
    i++;
  }
}

// Example with two arrays - no manifest bug
void array2_iter_ok(int array1[], int array2[]) {
  unsigned int i = 0;
  while (array1[i] != 0) {
    array2[i] = 42;
    i++;
  }
}

// Example with array and non-termination
/* Pulse-inf: find bug at widening 20 but not at widening 5 */
void array_iter_bad(int array[], int len) {
  int i = 0;
  while (i < len) {
    array[i] = 42;
    if (i > 10)
      i = 0;
    i++;
  }
}

// Iterate over an array - no bug
/* Pulse-Inf: works good */
void iterate_arraysize_ok(int array[256]) {
  unsigned int i = 0;
  while (i < (sizeof(*array) / sizeof(array[0]))) {
    array[i] = i;
    i++;
  }
}

// Iterate over an array using a bitmask to compute array value
/* Pulse-Inf: no bug - works good */
void iterate_bitmask_ok(int array[256], int len) {
  unsigned int i = 0;
  while (i < len) {
    array[i] = (i & (~7));
    i++;
  }
}

// Iterate over an array using a bitmask to compute array index
/* Pulse-Inf: no bug - works good */
void iterate_bitmask2_ok(int array[256], int len) {
  unsigned int i = 0;
  unsigned int j = 0;
  while (i < len) {
    j = (i & (~7));
    array[j] = i;
    i++;
  }
}

// Iterate over an array using a bitmask leading to a non-termination
/* Pulse-inf: able to find bug */
void FN_iterate_bitmask_bad(int array[256], unsigned int len) {
  unsigned int i = 0;
  while (i < len) {
    i = (i & (~7));
    array[i] = i;
    i++;
  }
}

// Simple bitshift test - will terminate as i will eventually reach 0
void bitshift_right_loop_ok(int i) {
  while (i)
    i = i >> 1;
}

// Simple bitshift test - will terminate as i will eventually reach 0
void FP_bitshift_left_loop_ok(int i) {
  while (i)
    i = i << 1;
}

/* To Test: This should terminate */
void bitshift_loop_ok(unsigned int i) {
  while (i % 2)
    i = (i << 1);
}

// Iterate over an array using a bitshift to compute array index leading to a
// non-termination
/* Pulse-inf: false negative. Unable to reason about bitshift */
void iterate_bitshift_bad(int array[256]) {
  unsigned int i = 1;
  while (i != 0) {
    array[i] = i;
    i = i << 1;
  }
}

// Iterate over an array using a bitshift to compute array index
/* Pulse-inf: no bug - good */
void iterate_bitshift1_ok(int array[256], int len) {
  unsigned int i = 1;
  while (i < len) {
    array[i] = i;
    i = i << 1;
  }
}

// Iterate over an array using a bitshift to compute array index
/* Pulse-inf: no bug - good */
void iterate_bitshift2_ok(int array[256], unsigned char i) {
  while (i != 0) {
    array[i] = i;
    i = i >> 1;
  }
}

// Integer test computing a condition that will never be true
void iterate_intoverflow_ok(int len) {
  unsigned int i = 0xFFFFFFFF;
  while (i != 0)
    i++;
}

// Iterate over an array using a modulo arithmetic leading to a bug
/* Pulse-infinite: false negative: unable to reason about unbounded index
 * stuttering in the loop */
/* To verify: this should work even with low widen threshold */
/* FALSE NEGATIVE */
void FN_iterate_modulus_bad(int array[256], unsigned int len, unsigned int i) {
  while (i < len) {
    i = i % 2;
    array[i] = i;
    i++;
  }
}

/* From: zlib */
/* Iterate computing a crc value - terminates no bug */
/* Pulse-inf: no bug - good */
#define W 8

void iterate_crc_ok() {
  unsigned int k;
  unsigned long crc0 = 0xFFFFFFFF;

  for (k = 1; k < W; k++) {
    crc0++;
  }
}

/* From: libpng */
/* Test from libpng with typedefs */
/* Expected result: no bug */
void png_palette_ok(int val) {
  int num;
  int i;
  int p = 0;

  if (val == 0)
    num = 1;
  else
    num = 10;

  for (i = 0; i < num; i++)
    p += val;
}

/* Peter O'Hearn's test - not terminate */
/* Pulse-Inf: OK - find bug  */
void simple_loop_equal_bad() {
  int x = 42;
  while (x == x)
    x = x + 1;
}

/* Pulse-Inf: OK! Find bug */
int compute_increment(int k) { return (k % 2 ? 1 : 0); }

void FN_loop_fcall_add_inductive_bad() {
  int i;
  int incr;
  for (i = 0; i < 10; i += incr)
    incr = compute_increment(i);
}

/* Used to be source of FP! not anymore */
/* Pulseinf now works good: no bug */
void allocate_all_in_array_ok(int* array[]) {
  for (int i = 0; i < 2; i++) {
    array[i] = malloc(sizeof(int));
  }
}

/* Infinite Goto in loop */
/* Pulseinf: FN */
void goto_in_loop_bad() {
  int i = 0;

  while (i < 10) {
  retry:
    if (i == 5)
      goto retry;
    i++;
  }
}

void goto_in_loop_without_eqtest_bad() {
  int i = 0;
  int j = 0;
  while (i < 10) {
  retry:
    j++;
    goto retry;
  }
}

/* Goto in loop */
/* FN is expected with pulse-widen-threshold < 4 */
void goto_cross_loop_stop_at_6_bad() {
  int i = 0;

retry:
  while (i < 10) {
    if (i == 6)
      goto retry;
    i++;
  }
}

/* Goto in loop */
/* Signal is expected with pulse-widen-threshold >= 3 */
void goto_cross_loop_stop_at_5_bad() {
  int i = 0;

retry:
  while (i < 10) {
    if (i == 5)
      goto retry;
    i++;
  }
}

/* Constant involved, on bug */
#define defined_const 8
int constant_loop_ok(int i, int j) {
  for (i = 0; i < defined_const; i++)
    j++;
  return (j);
}

/* in this example, the back-edge could also be considered as a loop-entry (if
 * b1=false) */
int nested_goto_bad(int b1, int b2, int b3, int b4) {
  int x = 0;
  if (b1) {
  retry:
    if (b2) {
      x++;
    }
    if (b2) {
      x++;
    }
  }
  if (b3) {
    if (b2) {
      x++;
    }
    if (b4) {
      goto retry;
    }
  }
}

void no_incr_first_iterations_ok() {
  int i = 0;
  int j = 0;
  while (i < 1) {
    i += j / 2;
    j++;
  }
}

void incr_if_eq(int k1, int k2, int* x) {
  if (k1 == k2) {
    (*x)++;
  }
}

void no_incr_first_iterations_interproc_ok() {
  int i = 0;
  int j = 0;
  while (i < 1) {
    incr_if_eq(j, 2, &i);
    j++;
  }
}
