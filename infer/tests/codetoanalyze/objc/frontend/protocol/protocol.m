/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@protocol Foo
- (void)fooMethod;

@property(retain) NSString* foo;
@end

@interface Bla : NSObject
- (void)fooMethod;
@end

@implementation Bla : NSObject

- (void)fooMethod {
  if ([self conformsToProtocol:@protocol(Foo)]) {
    return;
  }
}
@end
