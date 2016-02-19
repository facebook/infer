/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

CFBitVectorRef __cf_alloc(CFBitVectorRef);
CFBitVectorRef __cf_non_null_alloc(CFBitVectorRef);

CFBitVectorRef CFBitVectorCreate(CFAllocatorRef allocator,
                                 const UInt8* bytes,
                                 CFIndex numBits) {
  CFBitVectorRef c;
  return __cf_non_null_alloc(c);
}
