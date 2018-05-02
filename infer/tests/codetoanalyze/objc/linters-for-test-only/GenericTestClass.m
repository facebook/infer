// Copyright 2004-present Facebook. All Rights Reserved.

#import <Foundation/Foundation.h>

/* ======== My Base Class ======== */

@protocol MyBaseClassProtocol
@required
- (void)myBaseClassProtocolRequiredMethod;
@optional
- (void)myBaseClassProtocolOptionalMethod;
@end

@interface MyBaseClass : NSObject<MyBaseClassProtocol>

@property int myBaseClassProperty;
- (void)myBaseClassMethod;

@end

@interface MyBaseClass ()

- (void)myBaseClassInterfaceExtensionMethod;

@end

@implementation MyBaseClass

- (void)myBaseClassMethod {
}
- (void)myBaseClassInterfaceExtensionMethod {
}
- (void)myBaseClassProtocolRequiredMethod {
}
- (void)myBaseClassProtocolOptionalMethod {
}
- (int)myBaseClassProperty {
  return 0;
}
- (void)setMyBaseClassProperty:(int)value {
}

@end

@interface MyBaseClass (MyBaseClassCategory)

- (void)myBaseClassCategoryMethod;

@end

@implementation MyBaseClass (MyBaseClassCategory)

- (void)myBaseClassCategoryMethod {
}

@end

/* ======== My Subclass ======== */

@protocol MySubclassProtocol
@required
- (void)mySubclassProtocolRequiredMethod;
@optional
- (void)mySubclassProtocolOptionalMethod;
@end

@protocol MySubclassProtocol2
@required
- (void)mySubclassProtocol2RequiredMethod;
@optional
- (void)mySubclassProtocol2OptionalMethod;
@end

@protocol MySubclassSubprotocol<MySubclassProtocol>
@required
- (void)mySubclassSubprotocol2RequiredMethod;
@optional
- (void)mySubclassSubprotocol2OptionalMethod;
@end

@interface MySubclass : MyBaseClass<MySubclassSubprotocol, MySubclassProtocol2>

- (void)mySubclassMethod;

@end

@implementation MySubclass

- (void)myBaseClassMethod {
  [super myBaseClassMethod];
}

- (void)myBaseClassInterfaceExtensionMethod {
  [super myBaseClassInterfaceExtensionMethod];
}

- (void)myBaseClassProtocolRequiredMethod {
  [super myBaseClassProtocolRequiredMethod];
}

- (void)myBaseClassProtocolOptionalMethod {
  [super myBaseClassProtocolOptionalMethod];
}

- (int)myBaseClassProperty {
  return [super myBaseClassProperty];
}

- (void)setMyBaseClassProperty:(int)value {
  [super setMyBaseClassProperty:value];
}

- (void)myBaseClassCategoryMethod {
  [super myBaseClassCategoryMethod];
}

- (void)mySubclassMethod {
}
- (void)mySubclassProtocolRequiredMethod {
}
- (void)mySubclassProtocolOptionalMethod {
}
- (void)mySubclassProtocol2RequiredMethod {
}
- (void)mySubclassProtocol2OptionalMethod {
}
- (void)mySubclassSubprotocol2RequiredMethod {
}
- (void)mySubclassSubprotocol2OptionalMethod {
}

@end

@interface MySubclass (MySubclassCategory)

- (void)mySubclassCategoryMethod;

@end

@implementation MySubclass (MySubclassCategory)

- (void)mySubclassCategoryMethod {
}

@end
