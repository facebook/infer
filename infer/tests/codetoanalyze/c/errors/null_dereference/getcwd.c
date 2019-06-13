/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <unistd.h>

#define BUFFER_SIZE 16

char getcwd_ok() {
  char* cwd = getcwd(NULL, 0);
  if (cwd != NULL) {
    char result = cwd[0];
    free(cwd);
    return result;
  }
  char buf[BUFFER_SIZE];
  cwd = getcwd(&buf, BUFFER_SIZE);
  if (cwd != NULL) {
    return cwd[0];
  }
  return 'a';
}

char getcwd_no_buf_no_check_bad() {
  char* cwd = getcwd(NULL, 0);
  char result = cwd[0];
  free(cwd);
  return result;
}

char getcwd_no_buf_no_free_bad() {
  char* cwd = getcwd(NULL, 0);
  if (cwd != NULL) {
    return cwd[0];
  }
  return 'a';
}

char getcwd_no_check_bad() {
  char buf[BUFFER_SIZE];
  char* cwd = getcwd(&buf, BUFFER_SIZE);
  return cwd[0];
}
