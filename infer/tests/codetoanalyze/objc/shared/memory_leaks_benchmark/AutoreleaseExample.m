/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>

@interface Auto : NSObject {
  int x;
}
@property Auto* son;
@end

@implementation Auto

- (NSString*)autorelease_main {
  NSString* s = [NSString alloc];
  return [s autorelease];
}

@end

Auto* createA() {
  Auto* s1 = [[Auto alloc] init];
  return [s1 autorelease];
}

int autorelease_test1() {
  Auto* s1 = nil;
  Auto* s2 = nil;
  Auto* s3 = nil;
  @autoreleasepool {
    s1 = createA();
    [s1 retain];
    s2 = createA();
    s3 = createA();
  }
  return 0;
}

int autorelease_test2() {
  Auto* s1 = nil;
  Auto* s2 = nil;
  Auto* s3 = nil;
  @autoreleasepool {
    s1 = createA();
    s2 = createA();
    s3 = createA();
  }
  return 0;
}

void autorelease_test3() {
  NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
  NSString* string = [[NSString alloc] autorelease];
  // use the string
  [pool release];
  NSString* c = string;
}
