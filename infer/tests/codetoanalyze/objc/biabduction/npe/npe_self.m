/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>

@interface CSelf : NSObject {
  int x;
  CSelf* _currentCompositionState;
}
@property(nonatomic, copy, readonly) NSString* JSON;

@end

@implementation CSelf

- (id)init {
  self = [super init];
  self->x = 10;
  return self;
}

- (void)captureManagerSessionDidStart {
  __weak CSelf* weakSelf = self;
  CSelf* strongSelf = weakSelf;
  int x = strongSelf->x;
}

- (int)test {
  if (_currentCompositionState != nil) {
  }
  return _currentCompositionState->x;
}

- (BOOL)isEqual:(id)object {
  if (object == self)
    return YES;
  if (![object isKindOfClass:[self class]])
    return NO;
  CSelf* other = (CSelf*)object;
  return ([_JSON isEqualToString:other->_JSON]);
}

@end
