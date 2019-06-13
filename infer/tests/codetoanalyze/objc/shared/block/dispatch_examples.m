/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface DispatchEx : NSObject {
  int x;
}

@end

@implementation DispatchEx

- init {
  return self;
}

+ (int)dispatch_once_example {
  static DispatchEx* a = nil;
  // dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT,
  // 0),^{
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    a = [[DispatchEx alloc] init];
    a->x = 10;
  });
  return a->x;
}

+ (int)dispatch_async_example {
  static DispatchEx* a = nil;
  dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0),
                 ^{
                   a = [[DispatchEx alloc] init];
                   a->x = 10;
                 });
  return a->x;
}

+ (int)dispatch_after_example {
  static DispatchEx* a = nil;
  dispatch_after(dispatch_time(DISPATCH_TIME_NOW, (int64_t)(2 * NSEC_PER_SEC)),
                 dispatch_get_main_queue(),
                 ^{
                   a = [[DispatchEx alloc] init];
                   a->x = 10;
                 });
  return a->x;
}

+ (int)dispatch_group_example {
  static DispatchEx* a = nil;
  dispatch_group_async(NULL, dispatch_get_main_queue(), ^{
    a = [[DispatchEx alloc] init];
    a->x = 10;
  });
  return a->x;
}

+ (int)dispatch_group_notify_example {
  static DispatchEx* a = nil;
  dispatch_group_async(NULL, dispatch_get_main_queue(), ^{
    a = [[DispatchEx alloc] init];
    a->x = 10;
  });
  return a->x;
}

+ (int)dispatch_barrier_example {
  static DispatchEx* a = nil;
  dispatch_barrier_async(dispatch_get_main_queue(), ^{
    a = [[DispatchEx alloc] init];
    a->x = 10;
  });
  return a->x;
}

@end
