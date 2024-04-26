/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

typedef void (^MyBlock)();

@interface HeadlineView : NSObject

@end

@interface ImageView : NSObject

@property(nonatomic, readwrite, weak) id delegate;

@end

@implementation ImageView

@end

@implementation HeadlineView {
  ImageView* _headerImageView;
}

- (void)test_retain_cycle_good {
  MyBlock block = ^() {
    _headerImageView = [self _createHeaderImageView];
  };
}

- (ImageView*)_createHeaderImageView {
  ImageView* imageView = [[ImageView alloc] init];
  imageView.delegate = self;
  return imageView;
}

@end
