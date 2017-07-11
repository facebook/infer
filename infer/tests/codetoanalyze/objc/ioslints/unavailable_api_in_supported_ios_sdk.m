/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <UIKit/UIKit.h>
#import <AVFoundation/AVFoundation.h>

NS_CLASS_AVAILABLE(10_12, 10_0)

@interface Unav_class : NSObject

- (void)m;

@end

@interface Unavailable_api_in_supported_ios_sdk : NSObject

@end

@implementation Unavailable_api_in_supported_ios_sdk

- (void)test:(int)n and:(NSData*)data {
  NSDictionary* cacheData =
      [NSKeyedUnarchiver unarchiveTopLevelObjectWithData:data error:nil];
}

// bug
- (void)unsupported_class {
  AVPlayerLooper* looper =
      [[AVPlayerLooper alloc] initWithPlayer:nil
                                templateItem:nil
                                   timeRange:kCMTimeRangeInvalid];
  if (!looper) {
    NSLog(@"");
  }
}

// bug
- (void)unsupported_class_with_attributes:(nonnull Unav_class*)c {
  [c m];
}
@end

static NSDictionary* OpenURLOptionsFromSourceApplication(
    NSString* sourceApplication) {
  NSDictionary* options =
      @{UIApplicationOpenURLOptionsSourceApplicationKey : sourceApplication};
  return options;
}
