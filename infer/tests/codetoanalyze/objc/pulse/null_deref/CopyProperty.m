/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface View : NSObject
@end

@implementation View {
 @public
  int x;
}

@end

@interface Area : NSObject
@property(nonatomic, strong, nonnull) View* view;
@end

@implementation Area

- (id)copyWithZone:(NSZone*)zone {
  Area* area_copy = [[Area alloc] init];
  View* view = [View new];
  view->x = self->_view->x;
  area_copy->_view = view;
  return area_copy;
}

@end

@interface Space : NSObject

@property(nonatomic, copy) Area* area;

@end

@implementation Space
@end

void test_copy_with_zone_good() {
  Area* area = [Area new];
  area.view = [View new];
  area.view->x = 10;
  Space* space = [Space new];
  space.area = area;
  if (space.area.view->x != 10) {
    int* p = NULL;
    *p = 42;
  }
}

void test_copy_with_zone_bad() {
  Area* area = [Area new];
  area.view = [View new];
  area.view->x = 10;
  Space* space = [Space new];
  space.area = area;
  if (space.area.view->x == 10) {
    int* p = NULL;
    *p = 42;
  }
}
