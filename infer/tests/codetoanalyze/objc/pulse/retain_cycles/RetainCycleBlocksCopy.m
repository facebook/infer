/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

typedef void (^MyHandler)(NSString* name);

@interface Process : NSObject

@property(nonatomic, copy) MyHandler copied_handler;

@property(nonatomic, strong) MyHandler handler;

@property(nonatomic, strong) NSString* name;

@end

@implementation Process

- (void)retain_self_in_block_bad {
  self.handler = ^(NSString* b) {
    self->_name = b;
  };
}

- (void)retain_self_in_copied_block_good {
  self.copied_handler = ^(NSString* b) {
    self->_name = b;
  };
}

@end
