/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface AnalyticsTimeSpent : NSObject

@property(nonatomic, weak) id delegate;

@property(nonatomic, strong) id strong_delegate;

- (instancetype)initWithDelegate:(id)delegateObject;

- (instancetype)initWithStrongDelegate:(id)delegateObject;

- (instancetype)initWithRecord:(id)record;

@end
