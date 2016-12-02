/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <Foundation/Foundation.h>

typedef NS_ENUM(NSUInteger, SpacingEnum) {
  SpacingFill,
  SpacingCenter,
  SpacingLeading,
  SpacingTrailing,
};

@interface BoxedA : NSObject

@end

@implementation BoxedA {
  NSMutableDictionary* _spacingMap;
}

- (void)setMinimumHorizontalPadding:(SpacingEnum)horizontalSpacing
                                and:(SpacingEnum)spacingValue {
  NSNumber* key = @(horizontalSpacing);
  NSNumber* value = @(spacingValue);
  if ((_spacingMap[key] != value)) {
    _spacingMap[key] = value;
  }
}

// no null dereference because of models
- (void)test:(int)n {
  NSMutableDictionary* reactionsPerOffset = [NSMutableDictionary new];
  NSNumber* randomIndex = @(arc4random_uniform(n));
  reactionsPerOffset[randomIndex] =
      @([reactionsPerOffset[randomIndex] integerValue] + 1);
}

@end
