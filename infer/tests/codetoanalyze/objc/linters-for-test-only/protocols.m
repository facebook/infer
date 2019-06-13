/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@protocol A<NSObject>
@end

@protocol B<A>
@end

@protocol C<B>
@end

@protocol D<NSObject>
@end

@interface Foo : NSObject
+ (instancetype)newWithA:(id<A>)A; // A is a known "bad" protocol, so fire here
+ (instancetype)newWithB:(id<B>)B; // B inherits from A, so this line also fires
+ (instancetype)newWithC:(id<C>)C; // C eventually inherits from A; also fires
+ (instancetype)newWithCs:(NSArray<id<C>>*)Cs; // Collections should also fire
+ (void)newWithD:(id<D>)D; // D doesn't inherit from A; don't fire
@end
