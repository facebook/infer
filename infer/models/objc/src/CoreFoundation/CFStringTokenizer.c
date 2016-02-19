/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

CFStringTokenizerRef __cf_alloc(CFStringTokenizerRef);
CFStringTokenizerRef __cf_non_null_alloc(CFStringTokenizerRef);

CFStringTokenizerRef CFStringTokenizerCreate(CFAllocatorRef alloc,
                                             CFStringRef string,
                                             CFRange range,
                                             CFOptionFlags options,
                                             CFLocaleRef locale) {
  CFStringTokenizerRef c;
  return __cf_non_null_alloc(c);
}
