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

- (int)npe_bad {
  PInstance* object = [PInstance new];
  return [self get_ddclass_from:object] -> x;
}

+ (int)dispatch_async_block_npe_bad {
  dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0),
                 ^{
                   PInstance* object = [PInstance new];
                   DynamicDispatchMain* main = [DynamicDispatchMain new];
                   int x = [main get_ddclass_from:object] -> x;
                 });
  return 0;
}

@end
