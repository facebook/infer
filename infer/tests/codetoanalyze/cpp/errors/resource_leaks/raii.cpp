/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdio.h>

class OpenFile {
 public:
  OpenFile(const char* filename) { _filename = fopen(filename, "r"); }

  ~OpenFile() { fclose(_filename); }

 private:
  FILE* _filename;
};

void resource_leak(const char* filename) {
  char buffer[100];
  FILE* file = fopen(filename, "r");

  if (file == NULL)
    return;
  else
    fgets(buffer, 100, file);

  return;
}

void no_resource_leak(const char* filename) {
  OpenFile f(filename);
  return;
}
