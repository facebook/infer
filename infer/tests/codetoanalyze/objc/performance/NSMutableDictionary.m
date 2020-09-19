/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

// set element
void nsmutabledictionary_set_element_in_loop_linear(NSMutableDictionary* dict) {
  for (NSString* key in dict) {
    dict[key] = key;
  }
}
