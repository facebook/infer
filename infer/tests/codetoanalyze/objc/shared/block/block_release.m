/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

@interface My_manager : NSObject
- (int)my_method;

@end

@implementation My_manager

- (int)blockReleaseNoLeak {
  void (^b)(int a);
  int z = 3;
  CGContextRef context = CGBitmapContextCreate(NULL, 0, 0, 8, 0, 0, 0);
  CGImageRef newImage = CGBitmapContextCreateImage(context);
  b = ^(int a) {
    if (newImage)
      CGImageRelease(newImage);
  };
  b(z);
  if (context)
    CGContextRelease(context);
  return z;
}

@end
