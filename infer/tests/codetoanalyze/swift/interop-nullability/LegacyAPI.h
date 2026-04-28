/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wnullability-completeness"

#import <Foundation/Foundation.h>

// A macro that expands to a type-attached attribute. When such a macro appears
// after a property declarator, clang wraps the property type in a
// MacroQualifiedType layer (see clang/lib/Sema/SemaType.cpp), which today
// hides the inner _Nullable sugar from the AST exporter — fixed in the
// follow-up diff.
#define MY_TYPE_ATTR __attribute__((annotate("my_type_attr")))

@interface LegacyAPI : NSObject
// Missing nullability - Swift imports this as String!, prone to crashes.
- (NSString*)getUnannotatedString;

// Annotated _Nonnull - Swift imports as String.
- (NSString* _Nonnull)getNonnullString;

// Annotated _Nullable - Swift imports as String?.
- (NSString* _Nullable)getNullableString;

// @property with [nullable] AND a macro-attached type attribute. Today the
// [nullable] qualifier is invisible to SwiftObjCNullability and the property
// is wrongly flagged as unannotated — tracked here as
// macroAnnotatedNullableProp_good_FP, becomes _good in the follow-up diff.
@property(nullable, nonatomic, readonly)
    NSString* macroAnnotatedNullableProp MY_TYPE_ATTR;

// Same shape but unannotated - control case, must still be flagged.
@property(nonatomic, readonly)
    NSString* macroAnnotatedUnannotatedProp MY_TYPE_ATTR;
@end

#pragma clang diagnostic pop
