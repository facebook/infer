/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface DynamicDispatchClass : NSObject
@end

@implementation DynamicDispatchClass {
 @public
  int x;
}

@end

@protocol P

- (DynamicDispatchClass*)get_ddclass;

@end

@interface PInstance : NSObject<P>

@end

@implementation PInstance

- (DynamicDispatchClass*)get_ddclass {
  return nil;
}

@end

@interface DynamicDispatchMain : NSObject

@end

@implementation DynamicDispatchMain

- (DynamicDispatchClass*)get_ddclass_from:(id<P>)object {
  return [object get_ddclass];
}

- (int)dynamic_dispatch_npe_bad {
  PInstance* object = [PInstance new];
  return [self get_ddclass_from:object]->x;
}

- (DynamicDispatchClass*)get_ddclass_from_instance:(PInstance*)object {
  return [object get_ddclass];
}

- (int)no_dynamic_dispatch_npe_bad {
  PInstance* object = [PInstance new];
  return [self get_ddclass_from_instance:object]->x;
}

@end
