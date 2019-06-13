/*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

@interface RenderFooComponent : CKRenderComponent // good
@end

@interface RenderWithChildrenFooComponent
    : CKRenderWithChildrenComponent // good
@end

@interface RenderLayoutFooComponent : CKRenderLayoutComponent // good
@end

@interface RenderLayoutWithChildrenFooComponent
    : CKRenderLayoutWithChildrenComponent // good
@end
