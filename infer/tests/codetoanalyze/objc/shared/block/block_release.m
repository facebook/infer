/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

@interface My_manager : NSObject
- (int)my_method;

@end

@implementation My_manager

- (int)blockReleaseTODO {
  void (^b)(int a);
  int z = 3;
  CGContextRef context = CGBitmapContextCreate(NULL, 0, 0, 8, 0, 0, 0);
  CGImageRef newImage = CGBitmapContextCreateImage(context);
  b = ^(int a) {
    if (newImage)
      CGImageRelease(newImage);
  };
  b(z); // currently doesn't work due to PRECONDITION_NOT_MET here
  if (context)
    CGContextRelease(context);
  return z;
}

@end
