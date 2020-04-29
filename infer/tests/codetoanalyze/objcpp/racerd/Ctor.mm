/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>
#import <mutex>

@interface Ctor : NSObject
- (instancetype)init;
- (void)write_ok:(int)data;
@end

@implementation Ctor {
  std::mutex _mutex;
  int _data;
}

- (instancetype)init {
  if (!(self = [super init])) {
    return nil;
  }

  int i = _data;

  return self;
}

- (void)write_ok:(int)data {
  _mutex.lock();
  _data = data;
  _mutex.unlock();
}
@end
