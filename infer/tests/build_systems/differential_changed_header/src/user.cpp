/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "widget.h"

int use_widget() { return widget_get_bad(); }

// reported as fixed although unchanged: this file is analyzed for the
// procedures of widget.h in the previous version only, as the current version
// analyzes them in new_user.cpp
int user_bad() {
  int* p = nullptr;
  return *p;
}
