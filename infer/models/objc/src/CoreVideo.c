/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <CoreVideo/CVBuffer.h>
#import <CoreVideo/CVPixelBuffer.h>

void CVPixelBufferRelease(CVPixelBufferRef pxbuffer) { __free_cf(pxbuffer); }
