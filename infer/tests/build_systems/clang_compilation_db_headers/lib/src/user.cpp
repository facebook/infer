/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// not reported: with --suffix-match-changed-files, src/user.cpp is analyzed for
// the procedures of widget.h, and the path of this file ends with src/user.cpp,
// but this file does not include widget.h
int lib_user_bad() {
  int* p = nullptr;
  return *p;
}
