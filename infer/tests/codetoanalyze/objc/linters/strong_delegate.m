/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
