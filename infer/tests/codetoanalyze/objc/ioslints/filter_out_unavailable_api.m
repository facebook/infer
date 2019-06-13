/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <UIKit/UIKit.h>

@interface Filter_out_unavailable_api : NSObject

- (void)n NS_AVAILABLE(10_12, 10_0);

@end

@implementation Filter_out_unavailable_api

- (void)n {
}

// no bug
- (void)with_responds_to_selector:(Filter_out_unavailable_api*)a {
  [a n];
}

@end
