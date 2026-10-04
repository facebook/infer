/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "widget.h"

int use_widget(int* p) {
  Widget w;
  return w.header_method_ok(p) + w.header_method_bad() + header_function_bad();
}
