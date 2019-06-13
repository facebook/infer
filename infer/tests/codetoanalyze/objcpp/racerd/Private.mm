/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <mutex>
#import "Private.h"

@implementation Private {
  std::mutex _mutex;
  int _data;
  int _other_data;
}

// no report on _data
- (int)_private_read_ok {
  return _data;
}

- (void)write_ok:(int)data {
  _mutex.lock();
  _data = data;
  _mutex.unlock();
}

- (int)read_other_bad {
  return _other_data;
}

- (void)write_other_bad:(int)other_data {
  _mutex.lock();
  _other_data = other_data;
  _mutex.unlock();
}
@end
