/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import "SystemConfiguration/SCNetworkReachability.h"
#import <Foundation/Foundation.h>

SCNetworkReachabilityRef __cf_non_null_alloc(SCNetworkReachabilityRef);

SCNetworkReachabilityRef SCNetworkReachabilityCreateWithName(
    CFAllocatorRef allocator, const char* nodename) {
  SCNetworkReachabilityRef c;
  return __cf_non_null_alloc(c);
}

SCNetworkReachabilityRef SCNetworkReachabilityCreateWithAddress(
    CFAllocatorRef allocator, const struct sockaddr* address) {
  SCNetworkReachabilityRef c;
  return __cf_non_null_alloc(c);
}
