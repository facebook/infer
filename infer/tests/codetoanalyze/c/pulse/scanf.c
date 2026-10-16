/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

char* string_source();
void sink_int(int c);

void fscanf_writes_outputs_bad(FILE* f) {
  int i = 0;
  char s[16];
  s[0] = 0;
  if (fscanf(f, "%d %15s", &i, s) == 2 && i == 42 && s[0] == 'a') {
    int* p = NULL;
    *p = 42;
  }
}

int fscanf_pointer_output_ok(FILE* f) {
  int* p = NULL;
  if (fscanf(f, "%p", (void**)&p) == 1) {
    return *p;
  }
  return 0;
}

int fscanf_returns_eof_or_count_ok(FILE* f) {
  int i;
  int n = fscanf(f, "%d", &i);
  if (n < -1) {
    int* p = NULL;
    return *p;
  }
  return n;
}

void sscanf_writes_output_bad(const char* str) {
  int i = 0;
  if (sscanf(str, "%d", &i) == 1 && i == 42) {
    int* p = NULL;
    *p = 42;
  }
}

void sscanf_null_output_bad(const char* str) {
  int* p = NULL;
  sscanf(str, "%d", p);
}

void getenv_sscanf_bad() {
  int i;
  sscanf(getenv("SOME_VARIABLE"), "%d", &i);
}

void getenv_vsscanf_bad(va_list args) {
  vsscanf(getenv("SOME_VARIABLE"), "%d", args);
}

void getenv_vscanf_bad(va_list args) { vscanf(getenv("SOME_VARIABLE"), args); }

void scanf_writes_output_bad() {
  int i = 0;
  if (scanf("%d", &i) == 1 && i == 42) {
    int* p = NULL;
    *p = 42;
  }
}

void sscanf_propagate_taint_bad() {
  char* tainted = string_source();
  int i;
  if (sscanf(tainted, "%d", &i) == 1) {
    sink_int(i);
  }
}

int fscanf_failure_keeps_value_ok(FILE* f) {
  int i = 0;
  if (fscanf(f, "%d", &i) != 1) {
    if (i != 0) {
      int* p = NULL;
      return *p;
    }
  }
  return i;
}

int sscanf_n_skip_spaces_ok(const char* str) {
  int n;
  sscanf(str, " %n", &n);
  return n;
}

int sscanf_n_field_length_ok(const char* str) {
  int len;
  sscanf(str, "%*[^,]%n", &len);
  return len;
}

// %n assigns without being counted in the result
void sscanf_n_assigned_when_returning_zero_bad(const char* str) {
  int len = 0;
  if (sscanf(str, "%*[^,]%n", &len) == 0 && len > 0) {
    int* p = NULL;
    *p = 42;
  }
}

// only constant formats are inspected: with others, the outputs may be written
// on failure
int FP_sscanf_unknown_format_failure_keeps_value_ok(const char* str,
                                                    const char* format) {
  int i = 0;
  if (sscanf(str, format, &i) != 1) {
    if (i != 0) {
      int* p = NULL;
      return *p;
    }
  }
  return i;
}

// the format is not parsed, so every pointer argument is assumed to be written
void FP_sscanf_unused_null_arg_ok(const char* str) {
  int i;
  sscanf(str, "%d", &i, NULL);
}

// the format is not parsed, so a partial success is assumed to write every
// output
int FP_sscanf_partial_success_keeps_value_ok(const char* str) {
  int a = 0, b = 7;
  if (sscanf(str, "%d %d", &a, &b) != 2) {
    if (b != 7) {
      int* p = NULL;
      return *p;
    }
  }
  return a + b;
}
