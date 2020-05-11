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
  CGMutablePathRef path = CGPathCreateMutable();
  if (!path)
    return;
  CFRelease(path);
}

+ (void)cg_bitmap_context_create_image_no_leak_good {
  CGImageRef newImage = CGBitmapContextCreateImage(nil);
  CGImageRelease(newImage);
}

+ (void)cg_bitmap_context_create_image1_no_leak_good {
  CGImageRef newImage = CGBitmapContextCreateImage(nil);
  CFAutorelease(newImage);
}

- (void)bridge_no_leak_good {
  CFLocaleRef nameRef = CFLocaleCreate(NULL, NULL);
  NSLocale* a = CFBridgingRelease(nameRef);
}

- (void)no_bridge_leak_bad {
  CFLocaleRef nameRef = CFLocaleCreate(NULL, NULL);
}

- (void)bridge_transfer_no_leak_good {
  CFLocaleRef nameRef = CFLocaleCreate(NULL, NULL);
  NSLocale* locale = (__bridge_transfer NSLocale*)nameRef;
}

- (NSLocale*)ret_bridge {
  CFLocaleRef nameRef = CFLocaleCreate(NULL, NULL);
  return CFBridgingRelease(nameRef);
}

- (void)create_release_no_leak_ok {
  CFLocaleRef nameRef = CFLocaleCreate(NULL, NULL);
  CFRelease(nameRef);
}

- (void)call_bridge_no_leak_ok {
  NSLocale* locale = [self ret_bridge];
}

- (CFLocaleRef)ret_no_bridge {
  return CFLocaleCreate(NULL, NULL);
}

- (void)call_no_bridge_leak_bad {
  CFLocaleRef locale = [self ret_no_bridge];
}

NSLocale* wrap_bridge(CFLocaleRef x) { CFBridgingRelease(x); }

NSLocale* call_bridge_interproc_leak_ok_FP() {
  CFLocaleRef nameRef = CFLocaleCreate(NULL, NULL);
  NSLocale* locale = wrap_bridge(nameRef);
}

void wrap_cfrelease(CFLocaleRef x) { CFRelease(x); }

void call_cfrelease_interproc_leak_ok_FP() {
  CFLocaleRef nameRef = CFLocaleCreate(NULL, NULL);
  wrap_cfrelease(nameRef);
}

@end
