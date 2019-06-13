/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

void foo(int);

@interface A : NSObject
@end

@implementation A

- (void)myMethod:(id)anObj

{
  @synchronized(anObj)

  {
    // Everything between the braces is protected by the @synchronized
    // directive.
    int x = 0;
    x++;
    foo(x);
  }
}

@end
