/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface AClass : NSObject {
}
- (void)foo:(int)a;
- (int)bar:(int)a;
@end

@implementation AClass

- (void)foo:(int)a {
  a++;
}
- (int)bar:(int)a {
  return a++;
}

@end

void foo1(int a) { a++; }

int bar1(int a) { return a++; }

int main() {

  int x = 1;
  foo1(x);

  x = bar1(x);

  AClass* o = [AClass alloc];

  if (o) {

    [o foo:x];
    x = [o bar:x];
  }

  return 0;
}
