/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <CoreVideo/CVBuffer.h>
#import <CoreVideo/CVPixelBuffer.h>
#import <Foundation/NSObject.h>

CVPixelBufferRef MyCVPixelBufferCreate();

@interface CoreVideoExample : NSObject

@end

@implementation CoreVideoExample

- (void)cvpixelbuffer_released_no_leak {
  CVPixelBufferRef pxbuffer = MyCVPixelBufferCreate();
  CVPixelBufferRelease(pxbuffer);
}

- (void)cvpixelbuffer_not_released_leak {
  CVPixelBufferRef pxbuffer = MyCVPixelBufferCreate();
}
@end
