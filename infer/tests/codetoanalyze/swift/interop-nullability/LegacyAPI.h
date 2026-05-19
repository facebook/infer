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
// MacroQualifiedType layer (see clang/lib/Sema/SemaType.cpp). The AST exporter
// now descends through that wrapper so the inner [_Nullable] sugar is visible
// to Infer.
#define MY_TYPE_ATTR __attribute__((annotate("my_type_attr")))

@interface LegacyAPI : NSObject
// Missing nullability - Swift imports this as String!, prone to crashes.
- (NSString*)getUnannotatedString;

// Annotated _Nonnull - Swift imports as String.
- (NSString* _Nonnull)getNonnullString;

// Annotated _Nullable - Swift imports as String?.
- (NSString* _Nullable)getNullableString;

// @property with [nullable] AND a macro-attached type attribute. The
// [nullable] qualifier survives because the AST exporter looks through the
// MacroQualifiedType wrapper.
@property(nullable, nonatomic, readonly)
    NSString* macroAnnotatedNullableProp MY_TYPE_ATTR;

// Same shape but unannotated - control case, must still be flagged.
@property(nonatomic, readonly)
    NSString* macroAnnotatedUnannotatedProp MY_TYPE_ATTR;
@end

// A category interface gated by [#ifdef __swift__]. The macro is defined
// only by Swift's clang importer, so the [@interface] is invisible during
// regular ObjC compilation. The matching [@implementation] in the .m
// therefore has nothing to merge nullability from, and infer captures the
// impl method as bare [id] / [T*] with no [_Nullable]. Swift, in contrast,
// sees the annotated declaration and imports the method as returning [T?],
// so a Swift force-unwrap is against an [Optional] (deliberate by the
// caller) - infer should NOT fire MISSING_NULLABILITY_ANNOTATION.
#ifdef __swift__
@interface LegacyAPI (SwiftOnlyRefined)
- (nullable NSString*)swiftRefinedNullableString;
@end
#endif

// Test surface for shipping-readiness FP investigation. Each declaration
// pairs with a Swift call site in NullabilityTests.swift that asserts the
// checker does (or does not) fire on the patterns that show up in real
// fbobjc code.

// NS_ASSUME_NONNULL_BEGIN/END is the dominant convention for annotating
// modern ObjC headers. Inside the block, bare [T*] returns are treated as
// [_Nonnull] by Apple's clang importer, so Swift sees [T] (not [T?] or
// [T!]) and infer should agree.
NS_ASSUME_NONNULL_BEGIN
@interface AssumedNonnullAPI : NSObject
// Bare [NSString*] inside the block - implicitly [_Nonnull].
- (NSString*)getAssumedNonnullString;
// Explicit [nullable] override inside the block - the annotation wins.
- (nullable NSString*)getExplicitlyNullableInsideBlock;
@end
NS_ASSUME_NONNULL_END

// Factory / convention methods. Apple's clang importer applies special
// nullability rules to [init], [new], [alloc], etc. The checker should
// not fire on bare returns from these even when no explicit annotation
// is present.
@interface FactoryAPI : NSObject
- (instancetype)init;
+ (instancetype)new;
+ (instancetype)factoryInstance;
@end

// NSError** out-parameter convention: a [BOOL] (or other non-pointer)
// return paired with a trailing [NSError**] error parameter. The
// declaration's return type is not a pointer, so MISSING_NULLABILITY
// must not fire even though [NSError**] itself is unannotated.
@interface ErrorOutParamAPI : NSObject
- (BOOL)doThingWithError:(NSError**)error;
@end

// Class method (+) parity with instance methods (-): an unannotated
// pointer-returning class method should be reported just like an
// unannotated instance method.
@interface ClassMethodAPI : NSObject
+ (NSString*)classGetUnannotatedString;
@end

// Explicit [_Null_unspecified] modifier. Clang distinguishes this from the
// absence of any nullability annotation (the former wraps the type in an
// [AttributedType] with [TypeNullUnspecifiedAttrKind]; the latter has no
// wrapper), and the difference is a deliberate API contract: maintainers
// write the modifier to expose the return as a Swift implicitly-unwrapped
// Optional. The checker should treat it as annotated, not as missing.
@interface ExplicitlyUnspecifiedAPI : NSObject
- (NSString* _Null_unspecified)getExplicitlyUnspecifiedString;
@end

#pragma clang diagnostic pop
