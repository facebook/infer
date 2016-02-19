/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

CFSocketRef __cf_alloc(CFSocketRef);

CFSocketRef CFSocketCreate(CFAllocatorRef allocator,
                           SInt32 protocolFamily,
                           SInt32 socketType,
                           SInt32 protocol,
                           CFOptionFlags callBackTypes,
                           CFSocketCallBack callout,
                           const CFSocketContext* context) {
  CFSocketRef c;
  return __cf_alloc(c);
}
