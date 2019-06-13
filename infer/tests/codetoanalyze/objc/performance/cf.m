/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <CoreFoundation/CFArray.h>
#import <CoreFoundation/CFDictionary.h>

#include <stdint.h>

typedef uint32_t UInt32;

typedef int32_t SInt32;

void array_count_linear(CFArrayRef arr) {
  int32_t count = CFArrayGetCount(arr);
  for (int i = 0; i < count; ++i) {
  }
}

void dict_count_linear(CFDictionaryRef dict) {
  int32_t count = CFDictionaryGetCount(dict);
  for (int i = 0; i < count; ++i) {
  }
}

void cf_array_create_linear(int x) {
  int arrayValues[200];

  arrayValues[0] = 0;
  arrayValues[1] = 12;

  /* Now we can create the CF array... */
  CFArrayRef array = CFArrayCreate((CFAllocatorRef)NULL,
                                   (const void**)arrayValues,
                                   x,
                                   &kCFTypeArrayCallBacks);

  int32_t count = CFArrayGetCount(array);
  for (int i = 0; i < count; ++i) {
  }
}

void cf_array_create_copy_linear(int x) {
  int arrayValues[200];

  CFArrayRef array = CFArrayCreateCopy((CFAllocatorRef)NULL, arrayValues);

  int32_t count = CFArrayGetCount(array);
  for (int i = 0; i < count; ++i) {
  }
}
