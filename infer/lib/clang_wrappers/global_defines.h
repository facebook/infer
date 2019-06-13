/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// macro to help distinguish between infer compilation and normal one
#define __INFER__ 1

// set _FORTIFY_SOURCE to 0 to prevent it from changing some function prototypes
// https://securityblog.redhat.com/2014/03/26/fortify-and-you/
// We always do it when building models so we should do same thing
// when building any source code
#define _FORTIFY_SOURCE 0

// Make it harder for compilation to fail. When in the future, infer
// wants to use this information, it can call some builtin instead of
// the default static_assert
#define static_assert(...)
