/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface MyClass : NSObject

- (instancetype)initWithBlock:(void (^)(NSString*))block;

@end

@implementation MyClass {
  void (^_success_block)(NSString*);
  void (^_failure_block)(NSString*);
}

- (instancetype)initWithBlock:(void (^)(NSString*))block {
  if (self = [super init]) {
    _success_block = block;
    _failure_block = nil;
    return self;
  }
}

- (void)no_constructor_assignment:(void (^)(NSString*))block {
  _failure_block = block;
}

- (int)no_ivar_npe {
  self->_success_block(@"Yay"); // No IVAR_NOT_NULL_CHECKED reported because of
                                // the preanalysis.
  return 0;
}

- (int)ivar_npe {
  self->_failure_block(@"Failure!"); // IVAR_NOT_NULL_CHECKED reported.
  return 0;
}

@end
