/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int is_even(unsigned int x) { return ((x % 2) == 0); }

/* Bad: loop may not terminate */
void nonterm_is_even_simple_bad() {
  int x = 0;
  while ((x % 2) == 0)
    x += 2;
}

/* Bad: loop may not terminate (with param) */
void nonterm_is_even_param_bad(int x) {
  while ((x % 2) == 0)
    x += 2;
}

/* Bad: loop may not terminate (with function call) */
void nonterm_is_even_call_bad(int x) {
  while (is_even(x))
    x += 2;
}

/* Bad: loop may not terminate (with cross-iteration toggle) */
void nonterm_is_even_toggle_bad(int x) {
  int toggle = 1;
  while ((x % 2) == 0)
    if (toggle) {
      x += 2;
      toggle = 0;
    } else {
      x -= 2;
      toggle = 1;
    }
}

/* OK: loop never enters */
void terminate_is_even_simple_ok() {
  int x = 0;
  while ((x % 2) == 1)
    x += 2;
}

/* Bad: may not terminate if input is odd */
void nonterm_is_even_one_param_bad(unsigned int x) {
  while ((x % 2) == 1)
    x += 2;
}

/* OK: will terminate */
void terminate_is_odd_param_ok(unsigned int x) {
  while ((x % 2) == 1)
    x += 1;
}

/* Bad: may not terminate if input is odd */
void nonterm_is_not_even_call_bad(unsigned int x) {
  while (!is_even(x))
    x += 2;
}

/* OK: event toggle will make the loop break */
void FP_terminate_is_even_toggle_ok(unsigned int x) {
  int toggle = 1;
  while ((x % 2) == 0)
    if (toggle) {
      x += 1;
      toggle = 0;
    } else {
      x -= 2;
      toggle = 1;
    }
}

/* OK: second loop not entered */
void terminate_seq_loop_ok(unsigned int x) {
  while (x < 5)
    x++;
  while (x < 5)
    if (x == 3)
      x--;
    else
      x++;
}

/* Bad: second loop may not terminate */
void nonterm_seq_loop_bad(unsigned int x) {
  while (x < 4)
    x++;
  while (x < 5)
    if (x == 4)
      x--;
    else
      x++;
}

/* OK: each loop executes a finite number of times */
void terminate_nested_loop_ok(unsigned int x) {
  while (x < 5) {
    x++;
    while (x < 6)
      x++;
  }
}

/* Bad: second nested loop may not terminate due to predicate being true on
 * negative integer */
void nonterm_nested_loop_bad(int x) {
  while (x < 5) {
    x++;
    while (x < 6)
      x -= 2;
  }
}

/* OK: each loop executes a finite number of times */
void terminate_nested_seq_loop_ok(int x) {
  while (x < 5) {
    x++;
    while (x < 6)
      x++;
  }
  while (x < 10)
    x = (x * 2);
}

/* Bad: Last loop of the function may not terminate */
void nonterm_nested_seq_loop_bad(int x) {
  while (x < 5) {
    x++;
    while (x < 6)
      x++;
  }
  while ((x % 2) == 0)
    x += 2;
}

/* OK: state machine eventually returns */
void terminate_is_valid_state_ok(int state) {
  while (state != 5) {
    switch (state) {
      case 1:
        state = 2;
        break;
      case 2:
        state = 3;
        break;
      case 3:
        state = 4;
        break;
      case 4:
        state = 5;
      default:
        return;
    }
  }
}

/* Bad: State machine may loop infinitely */
void nonterm_stuttering_state_bad(int state) {
  while (state != 5) {
    switch (state) {
      case 1:
        state = 2;
        break;
      case 2:
        state = 1;
        break;
      case 3:
        state = 4;
        break;
      case 4:
        state = 5;
      default:
        return;
    }
  }
}

/* Simple linked list data structure def */
typedef struct s_ent {
  unsigned int val;
  struct s_ent* next;
} ent_t;

/* used in two tests below */
/* Currently: says it will not terminate */
void loop_entry_iter(ent_t* ent) {
  while (ent)
    ent = ent->next;
}

/* Never used - latent bug */
void nonterm_loop_unused_latent_bad(ent_t* ent) {
  while (ent)
    ent = ent->next;
}

/* OK: finite acylic list being iterated on will terminate */
void terminate_loop_finite_list_ok() {
  ent_t one;
  ent_t two;
  one.val = 1;
  one.next = &two;
  two.val = 2;
  two.next = 0;
  loop_entry_iter(&one);
}

/* Bad: cyclic loop being iterated until NULL may not terminate */
void FN_nonterm_loop_finite_list_bad() {
  ent_t one;
  ent_t two;
  one.val = 1;
  one.next = &two;
  two.val = 2;
  two.next = &one;
  loop_entry_iter(&one);
}

/* Recursive function may not terminate */
int FN_nonterm_rec_bad(int x) {
  if ((x % 2) == 0)
    return FN_nonterm_rec_bad(x + 2);
  return x;
}

/* Recursive function will always terminate */
int terminate_rec_ok(int x) {
  if ((x % 2) == 0)
    return terminate_rec_ok(x + 1);
  return x;
}

/* Recursive function may not terminate with conditionals */
int FN_inf_rec_deep_bad(int x) {
  if (x > 50 && x < 100)
    return FN_inf_rec_deep_bad(x + 4);
  else
    return FN_inf_rec_deep_bad(x + 2);
  return x;
}

/* Recursive function will always terminate with conditionals */
int inf_rec_deep_ok(int x) {
  if (x < 100)
    return inf_rec_deep_ok(x + 2);
  return x;
}

/* Collatz conjecture: it is unknown if this function returns for all input */
void collatz(int x) {
  while (x != 1) {
    if (x % 2)
      x = x * 3 + 1;
    else
      x = x / 2;
  }
}

/* Simplified modular arithmetic example similar to openssl CVE-2022-0778
 * non-termination bug */
/* There are some combinations of value for which the loop diverge */
/* Ex: p = 2, b = 4, e = 6 will not terminate */
void FN_ossl_mod_arith_toy_bad(int p, int b, int e) {
  while (p != 1) {
    p = (p * b) % e;
  }
}

/* Bad: Input for which function may not terminate: x = 9 */
void FN_nonterm_inductive_irreducible_cfg_bad(int x) {
  while (x != 0) {
  restart:
    x++;
    if (x == 10)
      goto restart;
    x -= 2;
  }
}

/* OK: Function will terminate for all input */
void terminate_inductive_irreducible_cfg_ok() {
  int x = 7;
  while (x > 0) {
  restart:
    x++;
    if (x == 10)
      goto restart;
    x -= 2;
  }
}

/* OK: is even remains true for every array element */
void terminate_inductive_array_ok() {
  int y[10] = {0x00};
  int i = 0;
  while ((y[i] % 2) == 0) {
    i = y[i] % 10;
    y[i] += 1;
  }
}

/* Bad: is even remains true for every array element */
void nonterm_inductive_array_bad() {
  int y[10] = {0x00};
  int i = 0;
  while ((y[i] % 2) == 0) {
    i = y[i] % 10;
    y[i] += 2;
  }
}
