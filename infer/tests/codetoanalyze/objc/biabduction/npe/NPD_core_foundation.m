/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import "NPD_core_foundation.h"
#import <CoreText/CTFramesetter.h>
#import <Security/SecKey.h>
#import <UIKit/UIKit.h>

@implementation NullDeref

// For now it doesn't contain memory leak by default. Later, if we remove
// CFRelease, there should be a leak.
- (void)layoutSubviews {
  UIView* attachmentContainerView = [UIView alloc];
  CGPathRef shadowPath =
      CGPathCreateWithRect(attachmentContainerView.bounds, NULL);
  CFRelease(shadowPath);
  [attachmentContainerView release];
}

+ (void)measureFrameSizeForTextNoLeak {
  CFMutableAttributedStringRef maString =
      CFAttributedStringCreateMutable(nil, 0);
  if (maString)
    CFRelease(maString);
}

+ (void)createCloseCrossGlyphNoLeak:(CGRect)rect {
  CGFloat lineThickness = 0.20f * CGRectGetHeight(rect);

  // One rectangle
  CGMutablePathRef path1 = CGPathCreateMutable();
  CFRelease(path1);
}

+ (void)test2 {
  SecKeyRef allowedPublicKey = SecTrustCopyPublicKey(nil);
  CFRelease(allowedPublicKey);
}

@end
