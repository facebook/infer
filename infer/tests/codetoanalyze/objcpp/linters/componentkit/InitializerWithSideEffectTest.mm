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

@interface FooComponent : CKCompositeComponent
@end

@implementation FooComponent
+ (instancetype)newDerp {
  dispatch_async( // error
      dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0),
      ^{
          // do something
      });
  dispatch_sync( // error
      dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0),
      ^{
          // do something
      });
  dispatch_after( // error
      DISPATCH_TIME_NOW,
      dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0),
      ^{
          // do something
      });
  // make sure nested things are caught
  ^{
    dispatch_sync( // error
        dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0),
        ^{
          ^{
            dispatch_async( // error
                dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0),
                ^{
                    // do something
                });
          }();
        });
  }();
  return nil;
}

- (void)derp {
  // not an error (only run on factory functions)
  dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0),
                 ^{
                     // do something
                 });
}
@end

@interface BarComponent : NSObject // not a real component
@end

@implementation BarComponent
+ (instancetype)newDerp {
  // not an error (only run on actual components)
  dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0),
                 ^{
                     // do something
                 });
  return nil;
}
@end
