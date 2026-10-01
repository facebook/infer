/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

char* string_source();
void sink_int(int);

long strtol_endptr_ok(const char* s) {
  char* end = NULL;
  long v = strtol(s, &end, 10);
  if (*end != '\0') {
    return -1;
  }
  return v;
}

unsigned long strtoul_endptr_ok(const char* s) {
  char* end = NULL;
  unsigned long v = strtoul(s, &end, 10);
  if (*end != '\0') {
    return 0;
  }
  return v;
}

double strtod_endptr_ok(const char* s) {
  char* end = NULL;
  double v = strtod(s, &end);
  if (*end != '\0') {
    return 0.0;
  }
  return v;
}

void strtol_endptr_not_null_ok(const char* s) {
  char* end = NULL;
  strtol(s, &end, 10);
  if (end == NULL) {
    int* p = NULL;
    *p = 42;
  }
}

long strtol_wrapper(const char* s, char** endp) { return strtol(s, endp, 10); }

long strtol_endptr_via_wrapper_ok(const char* s) {
  char* end = NULL;
  long v = strtol_wrapper(s, &end);
  if (*end != '\0') {
    return -1;
  }
  return v;
}

long strtol_null_endptr_ok(const char* s) { return strtol(s, NULL, 10); }

long strtol_null_endptr_via_wrapper_ok(const char* s) {
  return strtol_wrapper(s, NULL);
}

void strtol_no_digits_bad(const char* s) {
  char* end;
  strtol(s, &end, 10);
  if (end == s) {
    int* p = NULL;
    *p = 42;
  }
}

void strtol_some_digits_bad(const char* s) {
  char* end;
  strtol(s, &end, 10);
  if (end != s) {
    int* p = NULL;
    *p = 42;
  }
}

long strtol_chars_consumed(const char* s) {
  char* end;
  strtol(s, &end, 10);
  return end - s;
}

void strtol_chars_consumed_then_null_deref_bad(const char* s) {
  strtol_chars_consumed(s);
  int* p = NULL;
  *p = 42;
}

void strtol_freed_endptr_bad(const char* s, char** endp) {
  free(endp);
  strtol(s, endp, 10);
}

long strtol_null_str_bad() {
  const char* s = NULL;
  char* end;
  return strtol(s, &end, 10);
}

unsigned long strtoul_null_str_bad() {
  const char* s = NULL;
  char* end;
  return strtoul(s, &end, 10);
}

double strtod_null_str_bad() {
  const char* s = NULL;
  char* end;
  return strtod(s, &end);
}

void strtol_propagates_taint_bad() {
  char* end;
  long v = strtol(string_source(), &end, 10);
  sink_int(v);
}

void strtol_cursor_propagates_taint_bad() {
  char* p = string_source();
  strtol(p, &p, 10);
  long v = strtol(p, &p, 10);
  sink_int(v);
}

// FN: [*endptr] is modelled as a fresh pointer unrelated to [s], so reading
// through it after [s] is freed is not reported
char FN_strtol_endptr_use_after_free_bad(char* s) {
  char* end;
  strtol(s, &end, 10);
  free(s);
  return *end;
}
