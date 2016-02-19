/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Security/Security.h>

SecCertificateRef __cf_alloc(SecCertificateRef);

SecCertificateRef __cf_non_null_alloc(SecCertificateRef);

SecCertificateRef SecCertificateCreateWithData(CFAllocatorRef allocator,
                                               CFDataRef data) {
  SecCertificateRef c;
  return __cf_alloc(c);
}
