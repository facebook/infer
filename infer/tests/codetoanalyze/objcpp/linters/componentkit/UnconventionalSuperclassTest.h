/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

#import "FakeComponentKitHeader.h"

@interface FooComponent : CKCompositeComponent // good
@end

@interface BarComponent : FooComponent // bad
@end

@interface NTNativeTemplateComponent : CKCompositeComponent // fake NT
@end

@interface NativeTemplateComponent : NTNativeTemplateComponent // good
@end
