/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

NSArray* get_array() {
  NSArray* animals = [NSArray arrayWithObjects:@"cat", @"dog", nil];
  return @[ @"cat", @"dog" ];
}
