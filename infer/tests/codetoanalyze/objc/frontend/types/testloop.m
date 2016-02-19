/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

@interface FBScrollViewDelegateProxy : NSObject
@end

@implementation FBScrollViewDelegateProxy

typedef struct {
  float placeHolderWidth;
  float placeHolderHeight;
  float contentLeftSidePadding;
  float contentRightSidePadding;
  float additionalPlaceholderOffset;
  float contentGap;
} FBVideoAdLayout;

static const FBVideoAdLayout __iPadVideoAdLayout = {
    .placeHolderWidth = 554,
    .placeHolderHeight = 350,
    .contentLeftSidePadding = 140,
    .contentRightSidePadding = 60,
    .additionalPlaceholderOffset = 40,
    .contentGap = 11,
};

static const FBVideoAdLayout __iPhoneVideoAdLayout = {
    .placeHolderWidth = 244,
    .placeHolderHeight = 175,
    .contentLeftSidePadding = 20,
    .contentRightSidePadding = 20,
    .additionalPlaceholderOffset = 0,
    .contentGap = 7,
};

+ (FBVideoAdLayout)layoutToUse {
  return __iPhoneVideoAdLayout;
}

@end
