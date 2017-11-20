/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
 
 /* We model dispatch_once in a separate file rather than Dispatch.m because this code 
 doesn't compile together with Foundation. */
 
#import <Foundation/NSObject.h>

typedef void (^dispatch_block_t)(void);

typedef long dispatch_once_t;

static void _dispatch_once(dispatch_once_t* predicate, dispatch_block_t block) {
  block();
}
