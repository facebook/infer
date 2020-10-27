/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
@interface MyEnumerator : NSObject<NSFastEnumeration>
@end

void loop_enumerator_linear(MyEnumerator* enumeration) {
  for (id obj in enumeration) {
  }
}

void call_loop_enumerator_constant() {
  NSArray* arr = @[ @1, @2, @3 ];
  loop_enumerator_linear(arr);
}

// We get Top because front-end can't recognize NSFastEnumeration
void loop_id_enumerator_linear_FP(id<NSFastEnumeration> enumeration) {
  for (id obj in enumeration) {
  }
}
