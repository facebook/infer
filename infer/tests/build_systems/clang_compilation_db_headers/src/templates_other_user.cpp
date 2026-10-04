/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "templates.ipp"

long use_templates_long() { return template_function_bad<long>(); }

// reported when templates.ipp changes, although unchanged: this file is
// analyzed in full for template_function_bad<long>
int templates_other_user_bad() {
  int* p = nullptr;
  return *p;
}
