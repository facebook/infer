/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface NullReturnedByMethodA : NSObject

@end

@implementation NullReturnedByMethodA {
  int x;
}

- (NullReturnedByMethodA*)test {
  return nil;
}

- (int)test1 {
  return [self test] -> x;
}

@end
