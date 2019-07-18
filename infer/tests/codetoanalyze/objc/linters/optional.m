/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@protocol BarDelegate<NSObject>
@optional
- (void)optionalFunction;
@end

@interface Bar : NSObject
@property(weak, nonatomic) id<BarDelegate> delegate;
- (void)unsafeAction;
@end

@implementation Bar
- (void)unsafeAction {
  [self.delegate optionalFunction];
}

- (void)safeAction {
  id<BarDelegate> delegate = self.delegate;
  if ([delegate respondsToSelector:@selector(optionalFunction)]) {
    [self.delegate optionalFunction];
  }
}
@end

@interface Foo : NSObject<BarDelegate>
- (void)doSomething;
@end

@implementation Foo
- (void)doSomething {
  Bar* x = [Bar new];
  x.delegate = self;
  [x unsafeAction];
}
@end

int main(int argc, const char* argv[]) {
  Foo* y = [Foo new];
  [y doSomething];
  return 0;
}
