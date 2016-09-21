/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

@interface DispatchA : NSObject

@property(nonatomic) int x;

+ (instancetype)sharedInstance;

@end

@implementation DispatchA {
}

- init {
  return self;
}

+ (instancetype)sharedInstance {
  static dispatch_once_t once;
  static id sharedInstance;
  dispatch_once(&once, ^{
    sharedInstance = [[self alloc] init];
  });
  return sharedInstance;
}

+ (instancetype)trans {
  static id sharedInstance;
  void (^dummy_block)() = ^{
    sharedInstance = [[self alloc] init];
  };
  dummy_block();
  return sharedInstance;
}

+ (instancetype)dispatch_a_block_variable {
  static __typeof__([self new]) static_storage__;
  void (^initialization_block__)() = ^{
    static_storage__ = ([self new]);
  };
  static dispatch_once_t once_token__;
  _dispatch_once(&once_token__, initialization_block__);
  return static_storage__;
}

+ (instancetype)dispatch_a_block_variable_from_macro {
  return ({
    static __typeof__([self new]) static_storage__;
    void (^initialization_block__)() = ^{
      static_storage__ = ([self new]);
    };
    static dispatch_once_t once_token__;
    _dispatch_once(&once_token__, initialization_block__);
    static_storage__;
  });
}

+ (int)dispatch_a_block_variable_from_macro_delivers_initialised_object {
  DispatchA* a = [DispatchA dispatch_a_block_variable_from_macro];
  a->_x = 5;
  return 1 / (a->_x - 5);
}

@end

int DispatchMain() {
  DispatchA* b = [DispatchA sharedInstance];
  int* p = 0;
  if (b == 0)
    return *p;
  else
    return 0;
}
