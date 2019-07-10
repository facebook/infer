/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace some {
namespace thing {
using foo_int = int;
int* bad_ptr() {
  int* p = new (int);
  delete p;
  return p;
}
} // namespace thing
} // namespace some

// test that NamespaceAliasDecl is handled correctly
void deref_null_namespace_alias_ptr_bad() {
  namespace st = some::thing;
  st::foo_int x = 0;
  int* p = st::bad_ptr();
  *p = x;
}
