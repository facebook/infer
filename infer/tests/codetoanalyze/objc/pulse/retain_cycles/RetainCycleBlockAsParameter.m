/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>
#import <Foundation/NSData.h>

typedef void (^MyHandler)(NSData* newData);

@interface Fetcher : NSObject

@property(nonatomic, strong) MyHandler completionBlock;

- (instancetype)initWithCompletionBlock:(MyHandler)block;

@end

@implementation Fetcher

- (instancetype)initWithCompletionBlock:(_Nonnull MyHandler)block {
  _completionBlock = block;
  return self;
}

@end

@interface FBSomeDataManager : NSObject

- (void)setData:(NSData*)data;

@end

@implementation FBSomeDataManager {
  Fetcher* _fetcher;
  NSData* _data;
}

- (void)setData:(NSData*)data {
  _data = data;
}

- (void)fetchNewData_bad {
  // We retain fetcher
  _fetcher = [[Fetcher alloc] initWithCompletionBlock:^(NSData* newData) {
    // fetcher retains us
    [self setData:newData];
  }];
}

@end

int test_fetchNewData_bad() {
  FBSomeDataManager* a = [FBSomeDataManager new];
  [a fetchNewData_bad];
  return 0;
}
