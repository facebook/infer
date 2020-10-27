/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#import "MyEnumerator.h"

@implementation MyEnumerator {
  int n;
}

- (NSString*)nextObject {
  for (int i = 0; i < n; i++) {
  }
  // NSEnumerator.nextObject should not be replaced
  return [super nextObject];
}

@end
