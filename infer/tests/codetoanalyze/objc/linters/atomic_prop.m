/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface B : NSObject

- (void)foo;

@end

@implementation B

- (void)foo {
};

@end

@interface A : NSObject

@property(nonatomic) int* p;
@property int* q; // atomic by default
@property(atomic, assign) float f;

@property(strong) B* b;

- (void)write_p:(int)i;
- (int)read_p;
- (void)write_q:(int)i;
- (int)read_q;
- (void)write_f:(int)i;
- (int)read_f;
@end

@implementation A

@synthesize b;

- (A*)init {
  _p = 0; // Good access
  _q = 0; // Good access
  _f = 0; // Good access
}

- (A*)new {
  _p = 0; // Good access
  _q = 0; // Good access
  _f = 0; // Good access
}

- (A*)initWithBla:(int)d {
  _p = d; // Good access
  _q = d; // Good access
  _f = d; // Good access
}

- (A*)initWith:(int)e {
  _p = e; // Good access
  _q = e; // Good access
  _f = e; // Good access
}

- (void)dealloc {
  _q = 0; // Good access
}

- (void)writeP:(int)i {
  _p = i; // Good access
}

- (int)readP {
  int i = _q; // Bad access
  return _p; // Good access
}

- (void)writeQ:(int)i {
  _q = i; // Bad access
}

- (int)readQ {
  return _q; // Bad access
}

- (void)writeF:(float)j {
  self.f = j; // Good access
}

- (int)readF {
  return self.f; // Good access
}

- (void)bla {
  if (b) { // bad access
    [b foo]; // bad access
  }
}

- (void)accessWithinSynchronizedIsOk {
  @synchronized(self) {
    if (self->_f > 0) {
      self->_f += 1;
    }
  }
}

- (void)accessInBlock {
  void (^b)();
  b = ^() {
    self->_f += 1;
  };
  b();
}

@end
