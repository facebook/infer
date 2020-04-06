/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <UIKit/UIKit.h>

@interface MemoryLeaks

@property(strong) UIView* backgroundCoveringView;
@property(strong) UIView* attachmentContainerView;

@end

@implementation MemoryLeaks

- (void)cg_path_create_with_rect_no_leak_good {
  UIView* attachmentContainerView = [UIView alloc];
  CGPathRef shadowPath =
      CGPathCreateWithRect(attachmentContainerView.bounds, NULL);
  CGPathRelease(shadowPath);
}

- (void)cg_path_create_with_rect_leak_bad {
  CGPathRef shadowPath =
      CGPathCreateWithRect(self.backgroundCoveringView.bounds, NULL);
  self.backgroundCoveringView.layer.shadowPath = shadowPath;
}

+ (void)cg_path_create_mutable_leak_bad:(CGRect)rect {
  0.20f * CGRectGetHeight(rect);
  CGPathCreateMutable();
}

+ (void)cg_path_create_mutable_no_leak_good:(CGRect)rect {
  CGFloat lineThickness = 0.20f * CGRectGetHeight(rect);
  // One rectangle
  CGMutablePathRef path1 = CGPathCreateMutable();
  CFRelease(path1);
}

+ (void)cg_bitmap_context_create_image_no_leak_good {
  CGImageRef newImage = CGBitmapContextCreateImage(nil);
  CGImageRelease(newImage);
}

+ (void)cg_bitmap_context_create_image1_no_leak_good {
  CGImageRef newImage = CGBitmapContextCreateImage(nil);
  CFAutoRelease(newImage);
}

@end
