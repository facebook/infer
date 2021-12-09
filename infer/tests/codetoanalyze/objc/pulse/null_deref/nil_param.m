/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface NilParamA : NSObject {
 @public
  int x;
}
@end

@implementation NilParamA

- (NilParamA*)newObject {
  return [[NilParamA alloc] init];
}

- (NilParamA*)test:(NilParamA*)other {
  return [other newObject];
}

@end

// This test shows that when we call an Objective-C method with nil we don't
// call the method but we return nil instead.
int NilParamNpeBad() {
  NilParamA* a = [[NilParamA alloc] init];
  return [a test:nil]->x;
}
