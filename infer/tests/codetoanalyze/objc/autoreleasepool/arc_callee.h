/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface ArcCallee : NSObject

+ (ArcCallee*)allocObject;

+ (ArcCallee*)newObject;

+ (ArcCallee*)copyObject:(ArcCallee*)obj;

+ (ArcCallee*)mutableCopyObject:(ArcCallee*)obj;

+ (ArcCallee*)giveMeObject;

+ (int)giveMeInt;

+ (NSString*)giveTaggedPointerString;

+ (NSNumber*)giveTaggedPointerNumber;

+ (NSIndexPath*)giveTaggedPointerIndexPath;

+ (NSIndexSet*)giveTaggedPointerIndexSet;

+ (NSDate*)giveTaggedPointerDate;

@end
