/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface UselessClass : NSObject
- (int)a;
- (int)b;
@end

@implementation UselessClass
- (int)a {
  return 4;
}
- (int)b {
  return 5;
}
@end

static int add(int x, int y) { return x + y; }

@interface ClassWithPrivateMethods : NSObject
- (void)publicMethodThatDoesntUseIvars;
@end

@implementation ClassWithPrivateMethods {
  int _someNum;
  UselessClass* _someClass;
}

- (void)publicMethodThatDoesntUseIvars {
  NSLog(@"Hello World!");
}

- (void)_okayMethod1 {
  // This is fine because we are referencing an ivar.
  _someNum = 4;
}

- (void)_okayMethod2 {
  self->_someNum = 5;
}

- (int)_okayMethod3 {
  return add(_someClass.a, _someClass.b);
}

- (void)_badMethod1 {
  // This isn't referencing an ivar...there is no reason for this.
  NSLog(@"Hi Mom!");
}

- (void)_badMethod2 {
  ClassWithPrivateMethods* const c = [ClassWithPrivateMethods new];
  // This is unnecessary because we aren't referencing self.
  c->_someNum = 5;
}

@end
