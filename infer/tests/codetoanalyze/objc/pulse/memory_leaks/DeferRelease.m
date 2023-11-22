/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

#define DeferCFRelease(ref)                                     \
  CFTypeRef* cftype_defer __attribute__((cleanup(CFRelease))) = \
      (CFTypeRef*)&(ref)

@interface Image : NSObject

@end

@implementation Image

CGImageSourceRef _Nullable ImageExtractGainMap_no_leak_good(
    CGImageSourceRef imageSource, NSData* data) {
  const CGImageMetadataRef metadata =
      CGImageSourceCopyMetadataAtIndex(imageSource, 0, nil);
  DeferCFRelease(metadata);
}

CGImageSourceRef _Nullable ImageExtractGainMap_leak_bad(
    CGImageSourceRef imageSource, NSData* data) {
  const CGImageMetadataRef metadata =
      CGImageSourceCopyMetadataAtIndex(imageSource, 0, nil);
}

@end
