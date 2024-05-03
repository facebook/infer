/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface Component : NSObject
@end

@interface InternalComponent : Component
@end

@interface SpecialDelegate : NSObject
+ (instancetype)newWithComponent:(Component*)component;
@end

@implementation SpecialDelegate {
  __weak Component* _component;
}

+ (instancetype)newWithComponent:(Component*)component {
  SpecialDelegate* s = [super new];
  if (s) {
    s->_component = component;
  }
  return s;
}

@end

@implementation InternalComponent {
 @package
  SpecialDelegate* _delegate;
};

+ (void)test_no_retain_cycle_good {
  InternalComponent* c = [super new];
  SpecialDelegate* delegate = [SpecialDelegate newWithComponent:c];
  c->_delegate = delegate;
}

@end
