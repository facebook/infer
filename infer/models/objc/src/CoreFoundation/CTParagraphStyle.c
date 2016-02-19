/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <CoreText/CoreText.h>
#import <Foundation/Foundation.h>

CTParagraphStyleRef __cf_alloc(CTParagraphStyleRef);
CTParagraphStyleRef __cf_non_null_alloc(CTParagraphStyleRef);

CTParagraphStyleRef CTParagraphStyleCreate(
    const CTParagraphStyleSetting* settings, size_t settingCount) {
  CTParagraphStyleRef c;
  return __cf_alloc(c);
}
