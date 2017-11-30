/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

@interface ObervserExample : NSObject

@end

@implementation ObervserExample

- (void)observer_no_dead_store:(id)object {
  __block id observer = [[NSNotificationCenter defaultCenter]
      addObserverForName:nil
                  object:object
                   queue:nil
              usingBlock:^(NSNotification* note) {
                [[NSNotificationCenter defaultCenter] removeObserver:observer
                                                                name:nil
                                                              object:nil];
              }];
}

@end
