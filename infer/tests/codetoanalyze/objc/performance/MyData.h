/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface MyData : NSObject<NSCopying>

@property(nonatomic, readonly, copy) NSString* name;

@end
