/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@class Tracer;

struct _State {
  Tracer* tracer;

  virtual ~_State() { tracer = nil; }
};

@interface Animation : NSObject
- (Tracer*)tracer;
@end

@interface Animation () {
 @public
  struct _State* _state;
}

@end

@interface Tracer : NSObject
- (instancetype)initWithAnimation:(Animation*)a;
@end

@implementation Animation

- (Tracer*)tracer {
  _state = new _State();
  _state->tracer = [[Tracer alloc] initWithAnimation:self];
  return _state->tracer;
}

- (void)dealloc {
  NSLog(@"dealloc Animation");
}

@end

@implementation Tracer {
  _State* _state;
}

- (id)initWithAnimation:(Animation*)a {
  self = [super init];
  if (nil != self) {
    _state = a->_state;
  }
  return self;
}

- (void)dealloc {
  NSLog(@"dealloc Tracer");
}
@end

int main_bad() {
  Animation* a = [Animation new];
  Tracer* tracer = [a tracer];
  return 0;
}
