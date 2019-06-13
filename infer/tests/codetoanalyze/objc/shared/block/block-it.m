/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface MyBlock : NSObject

@end

@implementation MyBlock

- (void)array {

  NSArray* a = [[NSArray alloc] init];
  [a enumerateObjectsUsingBlock:^(id object, NSUInteger idx, BOOL* stop) {
    int ShouldStop;
    /*
     ... do something here...
     */
    if (ShouldStop) {
      *stop = YES;
    };
  }];
}

- (void)array_trans {

  NSArray* a = [[NSArray alloc] init];

  NSArray* objects = a;

  // The call to the iterator in the above function is translated as:
  void (^enumerateObjectsUsingBlock)(id, NSUInteger, BOOL*) =
      ^(id object, NSUInteger idx, BOOL* stop) {
        int ShouldStop;

        if (ShouldStop) {
          *stop = YES;
        };
      };
  BOOL* stop = malloc(sizeof(BOOL));
  *stop = NO;

  for (NSUInteger idx = 0; idx < objects.count; idx++) {

    id object = objects[idx];
    enumerateObjectsUsingBlock(object, idx, stop);
    if (*stop == YES)
      break;
  }
  free(stop);
}

@end
