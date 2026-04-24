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
// Missing nullability - Swift imports this as String!, prone to crashes.
- (NSString*)getUnannotatedString;

// Annotated _Nonnull - Swift imports as String.
- (NSString* _Nonnull)getNonnullString;

// Annotated _Nullable - Swift imports as String?.
- (NSString* _Nullable)getNullableString;
@end

#pragma clang diagnostic pop
