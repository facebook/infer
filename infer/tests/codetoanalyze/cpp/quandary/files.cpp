/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <iostream>
#include <fstream>
#include <unistd.h>

extern void __infer_url_sink(char*);

namespace files {

extern char* FLAGS_cli_string;

void read_file_call_exec_bad1(int length) {
  std::ifstream is("test.txt", std::ifstream::binary);
  if (is) {
    char* buffer = new char[length];
    is.read(buffer, length);
    execle(buffer, NULL);
    is.close();
  }
}

void read_file_call_exec_bad2(int length) {
  std::ifstream is("test.txt", std::ifstream::binary);
  if (is) {
    char* buffer = new char[length];
    is.readsome(buffer, length);
    execle(buffer, NULL);
    is.close();
  }
}

void read_file_call_exec_bad3(int length) {
  std::ifstream is("test.txt", std::ifstream::binary);
  if (is) {
    char* buffer = new char[length];
    is.getline(buffer, length);
    execle(buffer, NULL);
    is.close();
  }
}

// have to do matching on C procnames to make this work
void FN_read_file_call_exec_bad5(int length) {
  std::ifstream is("test.txt", std::ifstream::binary);
  if (is) {
    char* buffer = new char[length];
    is >> buffer;
    execle(buffer, NULL);
    is.close();
  }
}

// make sure we handle reads via iostreams too
void read_file_call_exec_bad5(std::iostream is, int length) {
  if (is) {
    char* buffer = new char[length];
    is.getline(buffer, length);
    execle(buffer, NULL);
  }
}

void read_file_from_flag_ok(int length) {
  std::ofstream file1(FLAGS_cli_string, std::ifstream::binary);
}

void url_from_flag_ok() { __infer_url_sink(FLAGS_cli_string); }

} // namespace files
