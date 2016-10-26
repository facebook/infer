/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSArray.h>

void bad1() {

  NSNumber* n2;
  int i;

  if (!n2) {
    i = 0;
  } else {
    i = 1;
  }
}

void bad2(NSArray* a, NSNumber* n) {
  int i = 0;
  for (; !n;) {
    i = n.intValue;
  }
}

void bad3(NSArray* a, NSNumber* n) {
  int i = 0;
  while (!n) {
    i = n.intValue;
  }
}

void good1(NSArray* a) {
  int i = 0;
  for (NSNumber* n in a) {
    i = n.intValue;
  }
}

void good2() {

  NSNumber* n2;
  int i;

  if (n2 != nil) {
    i = 1;
  } else {
    i = 0;
  }
}

int good3(NSNumber* number) {
  if (number.doubleValue) {
    return 0;
  }
  return 1;
}

//---

@interface Simple : NSObject

@property(strong, atomic) NSNumber* number;

- (NSNumber*)fooWithCondition:(BOOL)number andNumber:(NSNumber*)number;

@end

@implementation Simple

- (NSNumber*)fooWithCondition:(BOOL)condition andNumber:(NSNumber*)number {
  return number;
}

@end

//---

int bad4(NSNumber* number, Simple* simple) {
  if ([simple fooWithCondition:true andNumber:number] < 19) {
    return 0;
  }
  return 1;
}

int good4(NSNumber* number, Simple* simple) {
  if ([simple fooWithCondition:true andNumber:number].doubleValue) {
    return 0;
  }
  return 1;
}

int bad5(NSNumber* number, Simple* simple) {
  if ([simple fooWithCondition:(number ? 1 : 2) andNumber:number] != nil) {
    return 0;
  }
  return 1;
}

int bad6(NSNumber* number, Simple* simple) {
  if ([simple fooWithCondition:(number == nil ? 1 : 2) andNumber:number]) {
    return 0;
  }
  return 1;
}

int good5(NSNumber* number, Simple* simple) {
  if ([simple fooWithCondition:(number == nil ? 1 : 2) andNumber:number] !=
      nil) {
    return 0;
  }
  return 1;
}

int bad7(NSNumber* number) {
  if (number < 0) {
    return 0;
  }
  return 1;
}

int bad8(NSNumber* number) {
  if (number) {
    return 0;
  }
  return 1;
}

int bad9(NSNumber* number, Simple* simple) {
  simple.number = simple.number ? simple.number : number;
}

int bad10(NSNumber* number, Simple* simple) {
  if ([simple fooWithCondition:true andNumber:number]) {
    return 1;
  }
  return 0;
}

int good6(NSNumber* number) { return (number.integerValue > 5 ? 1 : 0); }
