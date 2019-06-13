/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void foo_with_default_visibility() __attribute__((visibility("default")));

// protected will appear as Default in the AST
void foo_with_protected_visibility() __attribute__((visibility("protected")));

// internal will appear as Hidden in the AST
void foo_with_internal_visibility() __attribute__((visibility("internal")));
void foo_with_hidden_visibility() __attribute__((visibility("hidden")));
void foo_with_used_attribute() __attribute__((used));
void foo() __attribute__((visibility("default"))) __attribute__((used));

void foo_with_default_visibility() {}
void foo_with_protected_visibility() {}
void foo_with_internal_visibility() {}
void foo_with_hidden_visibility() {}
void foo_with_used_attribute() {}
void foo() {}

void bar() {
  foo_with_default_visibility();
  foo_with_protected_visibility();
  foo_with_internal_visibility();
  foo_with_hidden_visibility();
  foo_with_used_attribute();
  foo();
}
