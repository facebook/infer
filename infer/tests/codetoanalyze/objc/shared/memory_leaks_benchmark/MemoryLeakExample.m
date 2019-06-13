/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import "MemoryLeakExample.h"
#import <CoreText/CTFramesetter.h>
#import <Security/SecKey.h>
#import <UIKit/UIKit.h>

@implementation MemoryLeakExample

// For now it doesn't contain memory leak by default. Later, if we remove
// CFRelease, there should be a leak.
- (void)layoutSubviews {
  UIView* attachmentContainerView = [UIView alloc];
  CGPathRef shadowPath =
      CGPathCreateWithRect(attachmentContainerView.bounds, NULL);
  // self.attachmentContainerView.layer.shadowPath = shadowPath;
  CGPathRelease(shadowPath);
  [attachmentContainerView release];
}

- (void)test {
  CGPathRef shadowPath =
      CGPathCreateWithRect(self.backgroundCoveringView.bounds, NULL);
  self.backgroundCoveringView.layer.shadowPath = shadowPath;
}

+ (void)measureFrameSizeForText {
  CFAttributedStringCreateMutable(nil, 0);
}

+ (void)measureFrameSizeForTextNoLeak {
  CFMutableAttributedStringRef maString =
      CFAttributedStringCreateMutable(nil, 0);
  CFRelease(maString);
}

+ (void)test1:(CFAttributedStringRef)str {
  CTFramesetterCreateWithAttributedString(str);
}

+ (void)test1NoLeak {
  CTFramesetterRef framesetter = CTFramesetterCreateWithAttributedString(nil);
  CFRelease(framesetter);
}

+ (void)createCloseCrossGlyph:(CGRect)rect {
  0.20f * CGRectGetHeight(rect);
  CGPathCreateMutable();
}

+ (void)createCloseCrossGlyphNoLeak:(CGRect)rect {
  CGFloat lineThickness = 0.20f * CGRectGetHeight(rect);

  // One rectangle
  CGMutablePathRef path1 = CGPathCreateMutable();
  CFRelease(path1);
}

+ (void)test2:(SecTrustRef)trust {
  SecTrustCopyPublicKey(trust);
}

+ (void)test2NoLeak {
  SecKeyRef allowedPublicKey = SecTrustCopyPublicKey(nil);
  CFRelease(allowedPublicKey);
}

+ (void)testImageRefRelease {
  CGImageRef newImage = CGBitmapContextCreateImage(nil);
  CGImageRelease(newImage);
}

CGColorRef FBColorCreateWithGray(CGFloat gray, CGFloat a);

- (id)testFBColorCreateWithGray {
  CGColorRef borderColor = FBColorCreateWithGray(0.0, 0.3);
  CGColorRelease(borderColor);
}

- (int)regularLeak {
  int* x = malloc(sizeof(int));
  *x = 7;
  return *x;
}

- (int)blockCapturedVarLeak {
  int* x = malloc(sizeof(int));
  *x = 2;
  int (^blk)(void) = ^() {
    return *x;
  };
  return blk();
}

- (int)blockFreeNoLeak {
  int* x = malloc(sizeof(int));
  *x = 2;
  int (^blk)(void) = ^() {
    int i = *x;
    free(x);
    return i;
  };
  return blk();
}

@end
