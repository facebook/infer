/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <UIKit/UIKit.h>

#define DECLARE_SUBCLASS(_class, _superclass) \
  @protocol __                                \
  ##_class;                                   \
  typedef _superclass<__##_class> _class;

@class CustomViewController;

typedef struct FuncTable {
  void (*_Nullable viewWillAppear)(CustomViewController* self, BOOL animated);
} FuncTable;

@interface CustomViewController : UIViewController

- (instancetype)initWithFuncTable:(FuncTable)funcTable;

@end

@implementation CustomViewController {
  FuncTable _funcTable;
}

- (instancetype)initWithFuncTable:(FuncTable)funcTable {
  if (self = [super initWithNibName:nil bundle:nil]) {
    _funcTable = funcTable;
  }
  return self;
}

- (void)viewWillAppear:(BOOL)animated {
  if (_funcTable.viewWillAppear) {
    _funcTable.viewWillAppear(self, animated);
  }
}

@end

DECLARE_SUBCLASS(MyCustomviewController, CustomViewController);

static void _ViewWillAppear(CustomViewController* self, BOOL animated) {
  if (animated) {
    int* p = NULL;
    int* n = *p;
  }
}

MyCustomviewController* MyCustomViewControllerCreate() {
  CustomViewController* viewController = [[CustomViewController alloc]
      initWithFuncTable:(FuncTable){
                            .viewWillAppear = _ViewWillAppear,
                        }];
  return (MyCustomviewController*)viewController;
}

int build_custom_view_controller_bad() {
  MyCustomviewController* controller = MyCustomViewControllerCreate();
  return 0;
}
