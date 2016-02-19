/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

CFErrorRef __cf_alloc(CFErrorRef);
CFErrorRef __cf_non_null_alloc(CFErrorRef);

CFErrorRef CFReadStreamCopyError(CFReadStreamRef stream) {
  CFErrorRef c;
  return __cf_alloc(c);
}

CFErrorRef CFWriteStreamCopyError(CFWriteStreamRef stream) {
  CFErrorRef c;
  return __cf_alloc(c);
}
