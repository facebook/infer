/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
