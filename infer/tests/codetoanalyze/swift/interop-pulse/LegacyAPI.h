/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wnullability-completeness"

#import <Foundation/Foundation.h>

@interface LegacyAPI : NSObject
// Unannotated -- Swift imports as String!.
- (NSString*)getUnannotatedString;

// Explicit _Nonnull -- Swift imports as String.
- (NSString* _Nonnull)getNonnullString;

// Explicit _Nullable -- Swift imports as String?.
- (NSString* _Nullable)getNullableString;
@end

NS_ASSUME_NONNULL_BEGIN
@interface AssumedNonnullAPI : NSObject
- (NSString*)getAssumedNonnullString;
- (nullable NSString*)getExplicitlyNullableInsideBlock;
@end
NS_ASSUME_NONNULL_END

@interface FactoryAPI : NSObject
- (instancetype)init;
+ (instancetype)new;
+ (instancetype)factoryInstance;
@end

@interface ErrorOutParamAPI : NSObject
- (BOOL)doThingWithError:(NSError**)error;
@end

#pragma clang diagnostic pop
