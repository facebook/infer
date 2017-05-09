/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <iostream>
#include <fstream>
#include <unistd.h>

namespace files {

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
}
