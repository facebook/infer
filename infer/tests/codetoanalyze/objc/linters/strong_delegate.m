/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface A : NSObject
- (void)bla;

@property(nonatomic, strong) id delegated;

@property(nonatomic, strong) id delegation;

@property(nonatomic, strong) id delegate;

@property(nonatomic, strong) id fileDelegate;

@property(nonatomic, strong) id delegateFile;

@property(nonatomic, strong) id OneDelegateFile;

@property(nonatomic, strong) id OneDelegateQueue;

@property(nonatomic, strong) id OneQueueInTheMiddleDelegate;

@end

@implementation A {
  BOOL _b;
}

- (void)bla {
}

@end
