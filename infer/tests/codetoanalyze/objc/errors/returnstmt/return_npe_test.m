/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface ContainerClass : NSObject {
 @public
  int containedValue;
}

@end

@implementation ContainerClass

@end

@interface MyClass : NSObject

@end

@implementation MyClass

- (void)aMethod:(ContainerClass*)c {
  int i = 0;
  if (c == nil) {
    return;
  }

  if (i == 0) {
    // here c cannot be nil, because of the previous if
    i = c->containedValue;
  }
}

@end
