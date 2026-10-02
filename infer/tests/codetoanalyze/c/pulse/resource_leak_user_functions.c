/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <string.h>

// functions named like the POSIX ones that Pulse models, with other types

static char* dup(const char* s) {
  size_t n = strlen(s) + 1;
  char* copy = malloc(n);
  if (copy) {
    memcpy(copy, s, n);
  }
  return copy;
}

void dup_user_function_ok(const char* s) { free(dup(s)); }

void dup_user_function_leak_bad(const char* s) { char* copy = dup(s); }

struct stage {
  int status;
  struct stage* next;
};

static int pipe(struct stage* s) { return s->status; }

int pipe_user_function_ok() {
  struct stage s = {0, NULL};
  return pipe(&s);
}

struct parser;

static int accept(struct parser* p, int token, void* ctx) { return token; }

void accept_user_function_ok(struct parser* p) { accept(p, 0, NULL); }
