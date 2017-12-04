/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <CoreVideo/CVBuffer.h>
#import <CoreVideo/CVPixelBuffer.h>

void CVPixelBufferRelease(CVPixelBufferRef pxbuffer) { __free_cf(pxbuffer); }
