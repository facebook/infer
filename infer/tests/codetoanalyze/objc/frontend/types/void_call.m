/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
