/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Security/SecKey.h>
#import <Security/SecTrust.h>

SecKeyRef __cf_alloc(SecKeyRef);

SecPolicyRef __cf_non_null_alloc(SecPolicyRef);

SecKeyRef SecTrustCopyPublicKey(SecTrustRef trust) {
  SecKeyRef c;
  return __cf_alloc(c);
}
