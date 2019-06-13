/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

class FBMemImage;

class FBMemQuery;

@interface FBMemModelUsed1 : NSObject

// NEW_COMPONENT_USING_MEM_MODEL bug
+ (instancetype)newImage:(FBMemImage*)image;
// no NEW_COMPONENT_USING_MEM_MODEL bug
+ (instancetype)newInt:(int)size;
// no NEW_COMPONENT_USING_MEM_MODEL bug
+ (int)newReturnInt:(FBMemImage*)size;
// NEW_COMPONENT_USING_MEM_MODEL bug
+ (instancetype)newMultiParams:(int)size
                         image:(FBMemImage*)image
                         query:(FBMemQuery*)query;

@end

@implementation FBMemModelUsed1

// NEW_COMPONENT_USING_MEM_MODEL bug
+ (instancetype)newImage:(FBMemImage*)image {
  return nil;
}

// no NEW_COMPONENT_USING_MEM_MODEL bug
+ (instancetype)newInt:(int)size {
  return nil;
}

// no NEW_COMPONENT_USING_MEM_MODEL bug
+ (int)newReturnInt:(FBMemImage*)size {
  return 0;
}

// NEW_COMPONENT_USING_MEM_MODEL bug
+ (instancetype)newMultiParams:(int)size
                         image:(FBMemImage*)image
                         query:(FBMemQuery*)query {
  return nil;
}

@end

@interface FBMemModelUsed2 : NSObject

+ (instancetype)newQuery:(FBMemQuery*)query;

@end

@implementation FBMemModelUsed2

+ (instancetype)newQuery:(FBMemQuery*)query {
  return nil;
}

@end
