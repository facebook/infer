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

void loop_id_enumerator_linear(id<NSFastEnumeration, NSCopying> enumeration) {
  for (id obj in enumeration) {
  }
}

void call_loop_id_enumerator_linear(NSArray* arr) {
  loop_id_enumerator_linear(arr);
}

@interface A : NSObject<NSCopying>
@end

void loop_enumerator_over_class_linear(A<NSFastEnumeration>* a) {
  for (id obj in a) {
  }
}
