/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import "MemoryLeakExample.h"
#import <UIKit/UIKit.h>
#import <CoreText/CTFramesetter.h>
#import <Security/SecKey.h>

@implementation MemoryLeakExample

//For now it doesn't contain memory leak by default. Later, if we remove
//CFRelease, there should be a leak.
- (void)layoutSubviews
{
    UIView *attachmentContainerView = [UIView alloc];
    CGPathRef shadowPath = CGPathCreateWithRect(attachmentContainerView.bounds, NULL);
    //self.attachmentContainerView.layer.shadowPath = shadowPath;
    CGPathRelease(shadowPath);
    [attachmentContainerView release];

}

- (void)test
{
    CGPathRef shadowPath = CGPathCreateWithRect(self.backgroundCoveringView.bounds, NULL);
    self.backgroundCoveringView.layer.shadowPath = shadowPath;
}

+ (void)measureFrameSizeForText
{
  CFMutableAttributedStringRef maString = CFAttributedStringCreateMutable(nil, 0);
}

+ (void)measureFrameSizeForTextNoLeak
{
  CFMutableAttributedStringRef maString = CFAttributedStringCreateMutable(nil, 0);
  CFRelease(maString);
}

+ (void)test1: (CFAttributedStringRef) str
 {
  CTFramesetterRef framesetter = CTFramesetterCreateWithAttributedString(str);
 }

 + (void)test1NoLeak
  {
   CTFramesetterRef framesetter = CTFramesetterCreateWithAttributedString(nil);
   CFRelease(framesetter);
  }

+ (void) createCloseCrossGlyph:(CGRect) rect
{
    CGFloat lineThickness = 0.20f * CGRectGetHeight(rect);

    // One rectangle
    CGMutablePathRef path1 = CGPathCreateMutable();
}

+ (void) createCloseCrossGlyphNoLeak:(CGRect) rect
{
    CGFloat lineThickness = 0.20f * CGRectGetHeight(rect);

    // One rectangle
    CGMutablePathRef path1 = CGPathCreateMutable();
    CFRelease(path1);
}

+ (void) test2: (SecTrustRef) trust
{
    SecKeyRef allowedPublicKey = SecTrustCopyPublicKey(trust);
}

+ (void) test2NoLeak
{
    SecKeyRef allowedPublicKey = SecTrustCopyPublicKey(nil);
    CFRelease(allowedPublicKey);
}

+ (void) testImageRefRelease
{
    CGImageRef newImage = CGBitmapContextCreateImage(nil);
    CGImageRelease(newImage);
}

CGColorRef FBColorCreateWithGray(CGFloat gray, CGFloat a);

- (id)testFBColorCreateWithGray
{
    CGColorRef borderColor = FBColorCreateWithGray(0.0, 0.3);
    CGColorRelease(borderColor);
}

@end
