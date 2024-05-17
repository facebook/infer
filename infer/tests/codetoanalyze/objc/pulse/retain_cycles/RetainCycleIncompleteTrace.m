/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <UIKit/UIKit.h>

@interface TestView : UIView
@end

@implementation TestView {
  UIView* _lastTouchedView;
  UIView* _timeRangeSelectorView;
}

- (void)_newGestureTypeFromGesture:(UIGestureRecognizer*)gesture {
  UIView* hitViewInScrubber = [self hitTest:[gesture locationInView:self]
                                  withEvent:nil];
  _lastTouchedView = hitViewInScrubber;
}

- (UIView*)hitTest:(CGPoint)point withEvent:(UIEvent*)event {
  UIView* const hitView = [super hitTest:point withEvent:event];
  if ((!hitView || hitView == self) && [self pointInside:point
                                               withEvent:event]) {
    return _timeRangeSelectorView;
  }
  return hitView;
}
@end
