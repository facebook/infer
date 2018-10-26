/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>
#import <mutex>

@interface Basic : NSObject
- (int)read;
- (void)_private_write_no_top_level_report:(int)data;
- (void)write:(int)data;
@end

@implementation Basic {
  std::mutex mutex_;
  int data_;
}

- (int)read {
  return data_;
}

- (void)_private_write_no_top_level_report:(int)data {
  data_ = data;
}

- (void)write:(int)data {
  mutex_.lock();
  [self _private_write_no_top_level_report:data];
  mutex_.unlock();
}
@end
