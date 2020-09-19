/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

@interface NSObject
@end

@protocol NSObject
@end

@protocol BarDelegate<NSObject>
@optional
- (void)optionalFunction;
@end

@interface Bar : NSObject
@property(nonatomic, assign) id<BarDelegate> delegate;
@end

@implementation Bar
- (void)unsafeAction {
  [self.delegate optionalFunction];
}

@end
