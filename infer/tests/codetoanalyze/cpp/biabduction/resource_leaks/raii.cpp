/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
