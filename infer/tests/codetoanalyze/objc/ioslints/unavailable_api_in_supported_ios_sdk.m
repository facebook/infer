/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

// no bug
- (void)test_no_bug:(int)n and:(NSData*)data {
  if (@available(macOS 10.13, iOS 11.0, *)) {
    NSDictionary* cacheData =
        [NSKeyedUnarchiver unarchiveTopLevelObjectWithData:data error:nil];
  }
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
// no bug
- (void)unsupported_class_with_check {
  if ([AVPlayerLooper class]) {
    AVPlayerLooper* looper =
        [[AVPlayerLooper alloc] initWithPlayer:nil
                                  templateItem:nil
                                     timeRange:kCMTimeRangeInvalid];
  }
}

// bug
- (void)unsupported_class_with_attributes:(nonnull Unav_class*)c {
  [Unav_class new];
}

// no bug
- (void)unsupported_class_with_attributes_with_check:(nonnull Unav_class*)c {
  if ([Unav_class class]) {
    [[Unav_class alloc] init];
  }
}

@end

static NSDictionary* OpenURLOptionsFromSourceApplication(
    NSString* sourceApplication) {
  NSDictionary* options =
      @{UIApplicationOpenURLOptionsSourceApplicationKey : sourceApplication};
  return options;
}
