/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <UIKit/UIKit.h>

@interface Unavailable_api_in_supported_ios_sdk : NSObject

@end

@implementation Unavailable_api_in_supported_ios_sdk

- (void)test:(int)n and:(NSData*)data {
  NSDictionary* cacheData =
      [NSKeyedUnarchiver unarchiveTopLevelObjectWithData:data error:nil];
}
@end

static NSDictionary* OpenURLOptionsFromSourceApplication(
    NSString* sourceApplication) {
  NSDictionary* options =
      @{UIApplicationOpenURLOptionsSourceApplicationKey : sourceApplication};
  return options;
}
