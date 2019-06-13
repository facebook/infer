/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
