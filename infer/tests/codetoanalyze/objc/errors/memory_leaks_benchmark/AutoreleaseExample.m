/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>

@interface A : NSObject {
  int x;
}
@property A* son;
@end

@implementation A

- (NSString*)main {
  NSString* s = [NSString alloc];
  return [s autorelease];
}

@end

A* createA() {
  A* s1 = [[A alloc] init];
  return [s1 autorelease];
}

int test1() {
  A* s1 = nil;
  A* s2 = nil;
  A* s3 = nil;
  @autoreleasepool {
    s1 = createA();
    [s1 retain];
    s2 = createA();
    s3 = createA();
  }
  return 0;
}

int test2() {
  A* s1 = nil;
  A* s2 = nil;
  A* s3 = nil;
  @autoreleasepool {
    s1 = createA();
    s2 = createA();
    s3 = createA();
  }
  return 0;
}

void test3() {
  NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
  NSString* string = [[NSString alloc] autorelease];
  // use the string
  [pool release];
  NSString* c = string;
}
