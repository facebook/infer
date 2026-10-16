/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stddef.h>
#include <string.h>

char strsep_first_token_ok(char* line) {
  char* s = line;
  char* tok = strsep(&s, ",");
  return tok[0];
}

char strsep_stack_buffer_ok() {
  char buf[8] = "a,b";
  char* s = buf;
  char* tok = strsep(&s, ",");
  return tok[0];
}

int strsep_loop_ok(char* line) {
  int n = 0;
  char* tok;
  while ((tok = strsep(&line, ",")) != NULL) {
    n += tok[0];
  }
  return n;
}

char strsep_second_token_bad(char* line) {
  char* s = line;
  strsep(&s, ",");
  char* tok = strsep(&s, ",");
  return tok[0];
}

char strsep_null_string_bad() {
  char* s = NULL;
  char* tok = strsep(&s, ",");
  return tok[0];
}

// the model ignores the contents of the string, which contains the delimiter
char FP_strsep_constant_string_ok() {
  char buf[] = "key=value";
  char* s = buf;
  strsep(&s, "=");
  char* value = strsep(&s, "=");
  return value[0];
}
