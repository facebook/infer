/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef int BOOL;
typedef unsigned int NSUInteger;

#define YES 1
#define NO 0
#define NULL ((void *)0)
#define nil ((id)0)

@interface NSObject

+ (instancetype)alloc;
+ (instancetype)new;
- (instancetype)autorelease;
- (instancetype)init;
- (Class)class;

- (BOOL)respondsToSelector:(SEL)s;
- (BOOL)conformsToProtocol:(Protocol *)p;

@end

@interface NSNumber : NSObject
+ (instancetype)numberWithInt:(int)x;
@end

@interface NSString : NSObject
- (NSUInteger)length;
@end

@interface NSDictionary : NSObject
+ (instancetype)dictionaryWithObjects:(const id[])objects
                              forKeys:(const id[])keys
                                count:(NSUInteger)count;
- (id)objectForKeyedSubscript:(id)key;
@end

@interface NSArray : NSObject
+ (instancetype)arrayWithObjects:(id *)objs count:(unsigned)cnt;
- (id)objectAtIndexedSubscript:(NSUInteger)index;
@end

@interface NSException : NSObject
@end

extern void NSLog(NSString *, ...);
