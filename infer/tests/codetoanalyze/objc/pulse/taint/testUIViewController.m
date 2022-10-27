/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <UIKit/UIKit.h>

@interface TestUIViewController : UIViewController
@property int number;
- (id)initWithNumber:(int)n;
- (int)getNumber;
- (void)setNumber:(int)n;
@end

@implementation TestUIViewController

- (id)initWithNumber:(int)n {
  self = [super initWithNibName:nil bundle:nil];
  if (self) {
    self.number = n;
  }
  return self;
}

- (void)setNumber:(int)n {
  self.number = n;
}

- (int)getNumber {
  return self.number;
}
@end

TestUIViewController* TestUIViewControllerCreate(int number) {
  return [[TestUIViewController alloc] initWithNumber:number];
}

void testUIViewController_initWithNibNameNPEBad() {
  TestUIViewController* view = TestUIViewControllerCreate(0);
  if (view) {
    int* p = NULL;
    *p = 0;
  }
}
