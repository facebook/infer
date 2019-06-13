/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* We model dispatch_once in a separate file rather than Dispatch.m because this
code doesn't compile together with Foundation. */

#import <Foundation/NSObject.h>

typedef void (^dispatch_block_t)(void);

typedef long dispatch_once_t;

static void _dispatch_once(dispatch_once_t* predicate,
                           __nonnull dispatch_block_t block) {
  block();
}
