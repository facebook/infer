/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <AVFoundation/AVPlayer.h>
#import <Foundation/NSDictionary.h>
#import <Photos/PHAssetResource.h>
#import <UIKit/UIImagePickerController.h>
#import <UIKit/UIKit.h>

@interface Unavailable_api_allowed_cases : NSObject

- (void)m NS_AVAILABLE(10_12, 10_0);

- (void)n NS_AVAILABLE(10_12, 10_0);

@property(nonatomic, strong) AVPlayer* player;

@end

#define CK_AT_LEAST_IOS9 (kCFCoreFoundationVersionNumber >= 1223.1)

#define AT_LEAST_IOS9 \
  (kCFCoreFoundationVersionNumber >= kCFCoreFoundationVersionNumber_iOS_9_0)

#ifndef kCFCoreFoundationVersionNumber_iOS_10_0
#define kCFCoreFoundationVersionNumber_iOS_10_0 1348
#endif

#ifndef kCFCoreFoundationVersionNumber_iOS_10_2
#define kCFCoreFoundationVersionNumber_iOS_10_2 1348.22
#endif

#define AT_LEAST_IOS10 \
  (kCFCoreFoundationVersionNumber >= kCFCoreFoundationVersionNumber_iOS_10_0)

@implementation Unavailable_api_allowed_cases

- (void)m {
}

- (void)n {
}

// no bug
- (void)with_responds_to_selector:(Unavailable_api_allowed_cases*)a {
  if ([a respondsToSelector:@selector(m)]) {
    int x = 1;
    [a m];
    x = 3;
  }
}
// no bug
- (void)with_responds_to_selector:(Unavailable_api_allowed_cases*)a
                              and:(BOOL)ok {
  if ([a respondsToSelector:@selector(m)] && ok) {
    [a m];
  }
}

// bug
- (void)without_responds_to_selector:(Unavailable_api_allowed_cases*)a {
  [a m];
}

// no bug
- (void)call_m:(Unavailable_api_allowed_cases*)a
    API_AVAILABLE(ios(10), macosx(10.13)) {
  int x = 1;
  [a m];
  x = 3;
}

// bug
- (void)with_responds_to_selector_in_else:(Unavailable_api_allowed_cases*)a {
  if ([a respondsToSelector:@selector(m)]) {
  } else {
    [a m];
  }
}

// no bug
- (void)with_responds_to_selector_nested_if:(Unavailable_api_allowed_cases*)a {
  if ([a respondsToSelector:@selector(m)]) {
    if ([a respondsToSelector:@selector(n)]) {
      [a m];
      [a n];
    }
  }
}

// no bug
- (void)with_instances_responds_to_selector {
  if ([[UICollectionView class]
          instancesRespondToSelector:@selector(setPrefetchingEnabled:)]) {
    [[UICollectionView appearance] setPrefetchingEnabled:NO];
  }
}

// bug
- (void)without_instances_responds_to_selector {
  [[UICollectionView appearance] setPrefetchingEnabled:NO];
}

// no bug
- (void)with_responds_to_selector_two_selectors:
    (Unavailable_api_allowed_cases*)a {
  if ([a respondsToSelector:@selector(m)] &&
      [a respondsToSelector:@selector(n)]) {
    [a m];
    [a n];
  }
}

// no bug
- (void)uifont_with_respondstoselector:(CGFloat)size {
  UIFont* font;
  if ([UIFont respondsToSelector:@selector(systemFontOfSize:weight:)]) {
    font = [UIFont systemFontOfSize:size weight:0];
  }
}

// bug
- (void)uifont_without_respondstoselector:(CGFloat)size {
  UIFont* font = [UIFont systemFontOfSize:size weight:0];
}

// no bug
- (void)m1 {
  NSDictionary* destinationPixelBufferAttributes;
  if (kCFCoreFoundationVersionNumber >=
      kCFCoreFoundationVersionNumber_iOS_9_0) {
    destinationPixelBufferAttributes =
        @{(NSString*)kCVPixelBufferOpenGLESTextureCacheCompatibilityKey : @YES};
  }
}

// bug
- (void)m2 {
  NSDictionary* destinationPixelBufferAttributes;
  destinationPixelBufferAttributes =
      @{(NSString*)kCVPixelBufferOpenGLESTextureCacheCompatibilityKey : @YES};
}

// bug
- (void)m3:(BOOL)ok {
  NSDictionary* destinationPixelBufferAttributes;
  if (ok) {
    destinationPixelBufferAttributes =
        @{(NSString*)kCVPixelBufferOpenGLESTextureCacheCompatibilityKey : @YES};
  }
}

// no bug
- (void)m4:(BOOL)ok {
  NSDictionary* destinationPixelBufferAttributes;
  if (kCFCoreFoundationVersionNumber >=
          kCFCoreFoundationVersionNumber_iOS_9_0 &&
      ok) {
    destinationPixelBufferAttributes =
        @{(NSString*)kCVPixelBufferOpenGLESTextureCacheCompatibilityKey : @YES};
  }
}

// no bug
- (void)m5 {
  NSDictionary* destinationPixelBufferAttributes;
  if (kCFCoreFoundationVersionNumber >=
          kCFCoreFoundationVersionNumber_iOS_9_0 &&
      kCFCoreFoundationVersionNumber >=
          kCFCoreFoundationVersionNumber_iOS_7_0) {
    destinationPixelBufferAttributes =
        @{(NSString*)kCVPixelBufferOpenGLESTextureCacheCompatibilityKey : @YES};
  }
}

// no bug
- (void)m6 {
  NSDictionary* destinationPixelBufferAttributes;
  if (AT_LEAST_IOS9) {
    destinationPixelBufferAttributes =
        @{(NSString*)kCVPixelBufferOpenGLESTextureCacheCompatibilityKey : @YES};
  }
}

// no bug
- (void)m7 {
  NSDictionary* destinationPixelBufferAttributes;
  if (CK_AT_LEAST_IOS9) {
    destinationPixelBufferAttributes =
        @{(NSString*)kCVPixelBufferOpenGLESTextureCacheCompatibilityKey : @YES};
  }
}

// no bug
- (void)m8 {
  NSDictionary* destinationPixelBufferAttributes;
  if (AT_LEAST_IOS10) {
    destinationPixelBufferAttributes =
        @{(NSString*)kCVPixelBufferOpenGLESTextureCacheCompatibilityKey : @YES};
  }
}

// no bug
- (void)playInReverse {
  if ([self.player respondsToSelector:@selector(playImmediatelyAtRate:)]) {
    [self.player playImmediatelyAtRate:-1.0];
  }
}

// no bug
- (PHAsset*)improper_ios_version_good:(NSDictionary*)info {
  PHAsset* videoAsset = NULL;
  if (@available(iOS 11, *)) { // not strictly correct version number, should be
                               // "11.0" We should handle this case anyway.
    videoAsset = [info objectForKey:UIImagePickerControllerPHAsset];
  }
  return videoAsset;
}

@end
