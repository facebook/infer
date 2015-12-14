/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

CFDictionaryRef __cf_alloc(CFDictionaryRef);
CFDictionaryRef __cf_non_null_alloc(CFDictionaryRef);

CFDictionaryRef CFPreferencesCopyMultiple(CFArrayRef keysToFetch,
                                          CFStringRef appName,
                                          CFStringRef user,
                                          CFStringRef host) {
  CFDictionaryRef c;
  return __cf_non_null_alloc(c);
}
