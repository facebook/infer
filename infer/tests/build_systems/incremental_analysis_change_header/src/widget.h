/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

// null dereference in the current version only: the incremental analysis of
// the previous version starts from the results of the current one, and must not
// reuse the summary of this procedure
inline int widget_get_bad(int* q) {
#ifdef CHANGED
  int* p = nullptr;
  return *p;
#else
  return q == nullptr ? 0 : *q;
#endif
}
