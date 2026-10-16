/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// hide the POSIX getline and getdelim from <stdio.h>
#define _POSIX_C_SOURCE 200112L

#include <stdio.h>

// same name as the POSIX getline but another signature
static int getline(char* s, int lim, FILE* f) {
  int c = fgetc(f);
  if (lim < 2 || c == EOF) {
    s[0] = '\0';
    return 0;
  }
  s[0] = c;
  s[1] = '\0';
  return 1;
}

int user_getline_first_char_ok(FILE* f) {
  char line[256];
  if (getline(line, sizeof(line), f)) {
    return line[0];
  }
  return 0;
}

void user_getline_summary_ok(FILE* f) {
  char line[4];
  line[0] = 'x';
  if (!getline(line, sizeof(line), f) && line[0] != '\0') {
    int* p = NULL;
    *p = 42;
  }
}

void user_getline_then_null_deref_bad(FILE* f) {
  char line[256];
  int* p = NULL;
  getline(line, sizeof(line), f);
  *p = 42;
}

// same name and first parameter as the POSIX getdelim but another signature
static int getdelim(char** s, int lim, int delim, FILE* f) {
  int c = fgetc(f);
  if (lim < 2 || c == EOF || c == delim) {
    return 0;
  }
  (*s)[0] = c;
  return 1;
}

void user_getdelim_keeps_pointer_ok(FILE* f) {
  char buf[4];
  char* s = buf;
  if (!getdelim(&s, sizeof(buf), ',', f) && s != buf) {
    int* p = NULL;
    *p = 42;
  }
}

void user_getdelim_then_null_deref_bad(FILE* f) {
  char buf[4];
  char* s = buf;
  int* p = NULL;
  getdelim(&s, sizeof(buf), ',', f);
  *p = 42;
}
