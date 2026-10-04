/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

// objc/pulse-nullability-annotations analyzes this file with
// --pulse-nullability-annotations

// methods and functions without a body: their declarations are all we know
__attribute__((objc_root_class))
@interface NonnullParams
- (void)takeNonnull:(NonnullParams* _Nonnull)x;
- (void)takeNonnullAttribute:(NonnullParams*)x __attribute__((nonnull));
@end

void c_function_nonnull_param(void* _Nonnull p);
void c_function_nonnull_attribute(void* p) __attribute__((nonnull));
void c_function_nonnull_object(NonnullParams* _Nonnull o);
void c_function_nonnull_block(void (^_Nonnull block)(void));

void nil_to_objc_method_nonnull_param_ok(NonnullParams* o) {
  NonnullParams* x = NULL;
  [o takeNonnull:x];
}

void nil_to_objc_method_nonnull_attribute_ok(NonnullParams* o) {
  NonnullParams* x = NULL;
  [o takeNonnullAttribute:x];
}

void nil_to_block_nonnull_param_ok(void (^block)(NonnullParams* _Nonnull)) {
  NonnullParams* x = NULL;
  block(x);
}

void null_to_c_function_nonnull_param_bad(void) {
  void* p = NULL;
  c_function_nonnull_param(p);
}

void null_to_c_function_nonnull_attribute_bad(void) {
  void* p = NULL;
  c_function_nonnull_attribute(p);
}

void nil_to_c_function_nonnull_object_bad(void) {
  NonnullParams* o = NULL;
  c_function_nonnull_object(o);
}

void nil_to_c_function_nonnull_block_bad(void) {
  void (^block)(void) = NULL;
  c_function_nonnull_block(block);
}

__attribute__((objc_root_class))
@interface NonnullCaller
@end

@implementation NonnullCaller

- (void)nullToCFunctionNonnullParamInMethodBad {
  void* p = NULL;
  c_function_nonnull_param(p);
}

@end
