/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface MyMutableObj : NSObject<NSMutableCopying>
@property(nonatomic, strong, nonnull) NSArray* mObjects;
@end

@implementation MyMutableObj

- (NSArray*)objects {
  return self.mObjects;
}

- (id)mutableCopyWithZone:(NSZone*)zone {
  MyMutableObj* copy = [[MyMutableObj alloc] init];
  if (copy != nil) {
    NSArray* arr = [NSArray new];
    [arr initWithArray:self.mObjects copyItems:YES];
    copy->_mObjects = arr;
  }
  return copy;
}

@end

void loop_over_mutable_copy_linear(MyMutableObj* b) {
  MyMutableObj* c = [b mutableCopy]; // calls into mutableCopyWithZone:
  NSArray* objects = c.objects;
  for (int i = 0; i < objects.count; i++) {
  }
}
