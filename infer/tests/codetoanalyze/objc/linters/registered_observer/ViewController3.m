/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface ViewControllerError : NSObject
@end
@implementation ViewControllerError

- (instancetype)init {
  if (self = [super init]) {
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(fired)
                                                 name:@"some_notification"
                                               object:nil];
  }
  return self;
}
@end
