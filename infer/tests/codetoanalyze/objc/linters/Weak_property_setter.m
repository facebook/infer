/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface Base : NSObject
@property(atomic, weak) id baseProp;
@property(atomic, weak, setter=setBaseAlternate:) Base* baseExplicitSetter;
@property(atomic, weak) id baseWillBeImplementedInCategory;
@end

@protocol Protocol
@property(atomic, weak) id protocolProp;
@end

@interface Derived : Base<Protocol>
@property(atomic, strong) id strongProp;
@property(atomic, weak) id derivedProp;
@property(atomic, weak) id willBeImplementedInCategory;

// No warnings for this; synthesized setters are OK.
@property(atomic, weak) NSObject* synthesized;

@property(atomic, weak) id unimplemented;
- (id)unimplemented;
// No warnings for this; this is not implemented here.
- (void)setUnimplemented:(id)value;
@end

@implementation Derived

// These should not generate warnings about custom weak setters:

// Not setters:

- (id)baseProp {
  return nil;
}

- (id)strongProp {
  return nil;
}

- (id)derivedProp {
  return nil;
}

- (id)protocolProp {
  return nil;
}

// A setter, but not a weak property.
- (void)setStrongProp:(id)value {
}

// The following should generate warnings:

- (void)setBaseProp:(id)value {
}

- (void)setBaseAlternate:(Base*)value {
}

- (void)setDerivedProp:(id)value {
}

- (void)setProtocolProp:(id)value {
}

@end

@interface Derived (DerivedCategory)
@property(atomic, weak) NSObject* derivedCategoryProp;
@end

@implementation Derived (DerivedCategory)

- (NSObject*)derivedCategoryProp {
  return nil;
}

// The following should generate warnings:

// Custom setter for a weak property in a category
- (void)setDerivedCategoryProp:(NSObject*)value {
}

// Custom setter for a weak property defined in a base class
- (void)setBaseWillBeImplementedInCategory:(id)value {
}

// Custom setter for a weak property defined in the class interface
- (void)setWillBeImplementedInCategory:(id)value {
}

@end
