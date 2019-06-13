/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <mutex>
#import <Foundation/NSObject.h>

@interface Basic : NSObject
- (int)read_bad;
- (void)write_bad:(int)data;
@end

@implementation Basic {
  std::mutex _mutex;
  int _data;
}

- (int)read_bad {
  return _data;
}

- (void)write_bad:(int)data {
  _mutex.lock();
  _data = data;
  _mutex.unlock();
}
@end
