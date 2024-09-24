/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface NilInsertionIntoCollectionBlocks : NSObject

@end

@implementation NilInsertionIntoCollectionBlocks

- (nullable NSDictionary<NSString*, id<NSSecureCoding>>*)
    calling_block_with_dispatch_good {
  __block NSData* serialization;
  dispatch_block_t serializeBlock = ^{
    serialization = [NSData new];
  };
  dispatch_sync(dispatch_get_main_queue(), serializeBlock);
  return @{NSStringFromClass([self class]) : serialization};
}

- (nullable NSDictionary<NSString*, id<NSSecureCoding>>*)
    calling_block_with_dispatch_async_good {
  __block NSData* serialization;
  dispatch_block_t serializeBlock = ^{
    serialization = [NSData new];
  };
  dispatch_async(dispatch_get_main_queue(), serializeBlock);
  return @{NSStringFromClass([self class]) : serialization};
}

- (nullable NSDictionary<NSString*, id<NSSecureCoding>>*)
    calling_block_with_dispatch_once_good {
  __block NSData* serialization;
  dispatch_block_t serializeBlock = ^{
    serialization = [NSData new];
  };
  static dispatch_once_t onceToken;
  dispatch_once(onceToken, serializeBlock);
  return @{NSStringFromClass([self class]) : serialization};
}

- (nullable NSDictionary<NSString*, id<NSSecureCoding>>*)calling_block_good {
  __block NSData* serialization;
  dispatch_block_t serializeBlock = ^{
    serialization = [NSData new];
  };
  serializeBlock();
  return @{NSStringFromClass([self class]) : serialization};
}

@end
