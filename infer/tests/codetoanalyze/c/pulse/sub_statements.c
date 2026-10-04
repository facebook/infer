/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

// a compiled-out logging macro, translated to a no-op call without a CFG node
#define LOG(msg) ((void)0)

void if_log_bad() {
  int c = 1;
  if (c)
    LOG("then");
  int* p = NULL;
  *p = 42;
}

void else_log_bad() {
  int c = 0;
  if (c) {
  } else
    LOG("else");
  int* p = NULL;
  *p = 42;
}

void label_log_bad() {
  goto out;
out:
  LOG("label");
  int* p = NULL;
  *p = 42;
}

void case_log_bad() {
  int k = 1;
  switch (k) {
    case 1:
      LOG("case");
  }
  int* p = NULL;
  *p = 42;
}

void default_log_bad() {
  int k = 2;
  switch (k) {
    case 1:
      break;
    default:
      LOG("default");
  }
  int* p = NULL;
  *p = 42;
}

void do_while_log_bad() {
  do
    LOG("do");
  while (0);
  int* p = NULL;
  *p = 42;
}

void for_log_bad() {
  for (int i = 0; i < 1; i++)
    LOG("for");
  int* p = NULL;
  *p = 42;
}

void while_log_bad() {
  int i = 0;
  while (i++ < 1)
    LOG("while");
  int* p = NULL;
  *p = 42;
}

static void log_if(int c) {
  if (c)
    LOG("then");
}

void call_log_if_bad() {
  log_if(1);
  int* p = NULL;
  *p = 42;
}

static void log_at_label(void) {
  goto out;
out:
  LOG("label");
}

void call_log_at_label_bad() {
  log_at_label();
  int* p = NULL;
  *p = 42;
}
