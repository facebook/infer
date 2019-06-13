/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma clang diagnostic ignored "-Wprotocol"

#pragma clang diagnostic ignored "-Wincomplete-implementation"

#pragma clang diagnostic ignored "-Wimplicit-function-declaration"

#import <Foundation/NSObject.h>

@implementation NSAutoreleasePool

- (id)init {
  return (id)[self autorelease];
}

@end
