/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface MyCopyObj : NSObject<NSCopying>
@property(nonatomic, strong, nonnull) NSArray* mObjects;
@end

@implementation MyCopyObj

- (NSArray*)objects {
  return self.mObjects;
}

- (id)copyWithZone:(NSZone*)zone {
  MyCopyObj* copy = [[MyCopyObj alloc] init];
  if (copy != nil) {
    NSArray* arr = [NSArray new];
    [arr initWithArray:self.mObjects copyItems:YES];
    copy->_mObjects = arr;
  }
  return copy;
}

@end

void loop_over_copied_objects_linear(MyCopyObj* b) {
  MyCopyObj* c = [b copy]; // calls into copyWithZone:
  NSArray* objects = c.objects;
  for (int i = 0; i < objects.count; i++) {
  }
}
