/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <mutex>
#import <Foundation/NSObject.h>

@interface LockInCallee : NSObject
- (int)read_bad;
- (void)write:(int)data;
@end

@implementation LockInCallee {
  std::mutex _mutex;
  int _data;
}

- (void)lock {
  _mutex.lock();
}

- (void)unlock {
  _mutex.unlock();
}

- (int)read_bad {
  return _data;
}

- (void)write:(int)data {
  [self lock];
  _data = data;
  [self unlock];
}
@end
