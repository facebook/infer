/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface CKComponent : NSObject

@end

@implementation CKComponent

@end

typedef CKComponent* (^FBSectionComponentGenerator)(id<NSObject> model,
                                                    id<NSObject> context);

struct TestComponentOptions {
  id componentGenerator;
};

@interface TestComponent : NSObject

+ (instancetype)newWithComponentGenerator:
    (FBSectionComponentGenerator)generator;
+ (instancetype)newWithOptions:(TestComponentOptions)options;

@end

@implementation TestComponent

+ (instancetype)newWithComponentGenerator:
    (FBSectionComponentGenerator)generator {
  return [self new];
}

+ (instancetype)newWithOptions:(TestComponentOptions)options {
  return [self new];
}

@end

__unused static void testBadValDecl() {
  NSObject* obj = [NSObject new];
  __unused TestComponent* component = [TestComponent
      newWithComponentGenerator:^CKComponent*(id<NSObject> m, id<NSObject> c) {
        __unused NSObject* innerObj = obj;
        return nil;
      }];
}

__unused static void testBadStructValDecl() {
  NSObject* obj = [NSObject new];
    __unused TestComponent *component =
    [TestComponent
     newWithOptions:{
         .componentGenerator = ^CKComponent *(id<NSObject> m, id<NSObject> c) {
             __unused NSObject *innerObj = obj;
             return nil;
}
,
}];
}

__unused static void testBadNonValDecl() {
  NSObject* obj = [NSObject new];
  __unused TestComponent* component = nil;
  component = [TestComponent
      newWithComponentGenerator:^CKComponent*(id<NSObject> m, id<NSObject> c) {
        __unused id<NSObject> innerObj = obj;
        return nil;
      }];
}

__unused static void testBadStructNonValDecl() {
  NSObject* obj = [NSObject new];
  __unused TestComponent* component = nil;
    component =
    [TestComponent
     newWithOptions:{
         .componentGenerator = ^CKComponent *(id<NSObject> m, id<NSObject> c) {
             __unused NSObject *innerObj = obj;
             return nil;
}
,
}];
}

__unused static void testOK() {
  __unused TestComponent* component = [TestComponent
      newWithComponentGenerator:^CKComponent*(id<NSObject> m, id<NSObject> c) {
        __unused id<NSObject> innerObj = m;
        return nil;
      }];
}

__unused static void testOKStruct() {
    __unused TestComponent *component =
    [TestComponent
     newWithOptions:{
         .componentGenerator = ^CKComponent *(id<NSObject> m, id<NSObject> c) {
             __unused id<NSObject> innerObj = m;
             return nil;
}
,
}];
}
