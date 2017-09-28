/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import "A.h"
#import "B.h"

@implementation B

- (int)npe_no_bad_footprint_in_getter:(A*)a {
  int* p = nil;
  NSData* metadata = a.metadata; // Doesn't crash here with Bad_footprint
  return *p; // NPE
}

- (int)npe_no_bad_footprint_in_setter:(A*)a andMetadata:(NSData*)metadata {
  int* p = nil;
  a.metadata = metadata; // Doesn't crash here with Bad_footprint
  return *p; // NPE
}
@end
