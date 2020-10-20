/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

void decode_object_for_key_linear(NSKeyedUnarchiver* keyed_unarchiver) {
  id v = [keyed_unarchiver decodeObjectForKey:@"key"];
  for (int i = 0; i < v; i++) {
  }
}
