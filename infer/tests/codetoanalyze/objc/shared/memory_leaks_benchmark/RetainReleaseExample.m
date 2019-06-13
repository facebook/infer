/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface RRA : NSObject

@end

@implementation RRA

- init {
  return self;
}

@end

void retain_release_test() {
  RRA* a = [[RRA alloc] init];
  [a retain];
  [a release];
}
