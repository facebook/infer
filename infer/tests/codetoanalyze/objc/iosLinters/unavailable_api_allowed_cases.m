/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <UIKit/UIKit.h>

@interface Unavailable_api_allowed_cases : NSObject

- (void)m NS_AVAILABLE(10_12, 10_0);

- (void)n NS_AVAILABLE(10_12, 10_0);

@end

@implementation Unavailable_api_allowed_cases

- (void)m {
}

- (void)n {
}

// no bug
- (void)with_responds_to_selector:(Unavailable_api_allowed_cases*)a {
  if ([a respondsToSelector:@selector(m)]) {
    int x = 1;
    [a m];
    x = 3;
  }
}
// no bug
- (void)with_responds_to_selector:(Unavailable_api_allowed_cases*)a
                              and:(BOOL)ok {
  if ([a respondsToSelector:@selector(m)] && ok) {
    [a m];
  }
}

// bug
- (void)without_responds_to_selector:(Unavailable_api_allowed_cases*)a {
  [a m];
}

// bug
- (void)with_responds_to_selector_in_else:(Unavailable_api_allowed_cases*)a {
  if ([a respondsToSelector:@selector(m)]) {
  } else {
    [a m];
  }
}

// no bug
- (void)with_responds_to_selector_nested_if:(Unavailable_api_allowed_cases*)a {
  if ([a respondsToSelector:@selector(m)]) {
    if ([a respondsToSelector:@selector(n)]) {
      [a m];
      [a n];
    }
  }
}

// no bug
- (void)with_responds_to_selector_two_selectors:
    (Unavailable_api_allowed_cases*)a {
  if ([a respondsToSelector:@selector(m)] &&
      [a respondsToSelector:@selector(n)]) {
    [a m];
    [a n];
  }
}

// no bug
- (void)uifont_with_respondstoselector:(CGFloat)size {
  UIFont* font;
  if ([UIFont respondsToSelector:@selector(systemFontOfSize:weight:)]) {
    font = [UIFont systemFontOfSize:size weight:0];
  }
}

// bug
- (void)uifont_without_respondstoselector:(CGFloat)size {
  UIFont* font = [UIFont systemFontOfSize:size weight:0];
}

@end
