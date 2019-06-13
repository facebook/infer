/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface D : NSObject

@end

typedef void (^MyHandler)(D* data);

@interface B : NSObject

- (void)sHandler:(MyHandler)h;
@end

@implementation B {
  D* _d;
  MyHandler _h;
}

- (void)sHandler:(MyHandler)h {

  self->_h = h;
}

@end

@interface A : NSObject

- (void)capture;
@end

@implementation A {
  B* _b;
  D* _data;
}

- (void)capture {
  _b = [B alloc];
  [_b sHandler:^(D* d) {
    _data = d;
  }];
}

@end

A* foo(A* a) {

  [a capture];

  return a;
};

int main(int argc, const char* argv[]) {

  A* a = [A alloc];

  a = foo(a);

  return 0;
}
