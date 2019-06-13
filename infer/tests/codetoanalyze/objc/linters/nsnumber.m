/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

void bad1(NSNumber* isNum) {
  if (isNum) {
  }
}

void bad2(NSNumber* isNum) {
  if (!isNum) {
  }
}

void bad3(NSNumber* number1, NSNumber* number2) {
  if (number1 == nil || number2) {
  }
}

@interface TestClass : NSObject

@property(atomic) int value;

- (BOOL)doStuff:(NSNumber*)number;

@end

@implementation TestClass

- (BOOL)doStuff:(NSNumber*)number {
  return YES;
}

@end

void bad4(NSNumber* number, TestClass* t) { t.value = number ? 1 : 0; }

void ok1(NSNumber* number, TestClass* t) { t.value = number == nil ? 1 : 0; }

void ok2(NSNumber* number, TestClass* t) {
  if ([t doStuff:number]) {
  }
}

void ok3(NSNumber* isNum) {
  if (isNum != nil) {
  }
}

void ok4(NSNumber* isNum) {
  if (nil != isNum) {
  }
}

void ok5(NSNumber* isNum) {
  if (nil == isNum) {
  }
}

void ok6(NSNumber* isNum) {
  if (isNum == nil) {
  }
}

void accessor_ok1(NSNumber* num) {
  if (![num boolValue]) {
  }
}

void accessor_ok2(NSNumber* num) {
  if ([num boolValue]) {
  }
}
