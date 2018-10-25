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
- (void)write:(int)data;
@end

@implementation Basic {
  std::mutex mutex_;
  int data_;
}

- (int)read {
  return data_;
}

- (void)write:(int)data {
  mutex_.lock();
  data_ = data;
  mutex_.unlock();
}
@end
