/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

typedef struct LSIvarsContext {
  void* ivars;
} LSIvarsContext;

typedef struct Ivars {
  void (*dealloc)(struct Ivars*);
  NSString* str;
} Ivars;

@interface T : NSObject
@end
@implementation T

- (void)m {
  LSIvarsContext __blablaContext;
  Ivars* blabla = (Ivars*)__blablaContext.ivars;

  dispatch_async(dispatch_get_global_queue(0, 0), ^{
    blabla->str = @"str";
  });
}
@end
