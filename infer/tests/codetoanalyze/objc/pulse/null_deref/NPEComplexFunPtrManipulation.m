/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@class FunPtrManipulatedContent;

@interface FunPtrExample : NSObject

@property FunPtrManipulatedContent* manipulatedContent;

- (void)update_manipulatedContent:(NSObject*)object;

@end

@interface FunPtrManipulationEntry : NSObject

@end

@interface WithFunPtrCallback : NSObject

- (void)apply_callback:(void (*)(NSObject*, FunPtrManipulatedContent*))callback
               context:(NSObject*)context
                   arg:(FunPtrManipulatedContent*)arg;

@end

@interface FunPtrObjectHandler : NSObject {
  NSObject* _object;
  void (*_updateCallback)(FunPtrManipulatedContent*);
}

@property WithFunPtrCallback* withCallback;

- (instancetype)initWithObject:(NSObject*)object
                updateCallback:
                    (void (*)(FunPtrManipulatedContent*))updateCallback;

@end

@implementation FunPtrManipulationEntry {
  NSObject* _obj;
}

- (void)pass_obj:(void (*)(FunPtrExample*, NSObject*))completion
         context:(FunPtrExample*)context {
  (*completion)(context, _obj);
}

@end

@implementation WithFunPtrCallback

- (void)apply_callback:(void (*)(FunPtrObjectHandler*,
                                 FunPtrManipulatedContent*))callback
               context:(FunPtrObjectHandler*)context
                   arg:(FunPtrManipulatedContent*)arg {
  (*callback)(context, arg);
}

@end

@implementation FunPtrObjectHandler

- (instancetype)initWithObject:(NSObject*)object
                updateCallback:
                    (void (*)(FunPtrManipulatedContent*))updateCallback {
  if (self = [super init]) {
    _object = object;
    _updateCallback = updateCallback;
  }
  return self;
}

void call_updateCallback(FunPtrObjectHandler* context,
                         FunPtrManipulatedContent* arg) {
  (*context->_updateCallback)(arg);
}

- (void)apply_updateCallback:(FunPtrManipulatedContent*)arg {
  WithFunPtrCallback* withCallback = _withCallback;
  [_withCallback apply_callback:&call_updateCallback context:self arg:arg];
}

@end

id FunPtrObjectHandlerForObject(
    NSObject* object,
    void (*updateCallback)(FunPtrManipulatedContent*),
    FunPtrManipulatedContent* context) {
  FunPtrObjectHandler* objectHandler =
      [[FunPtrObjectHandler alloc] initWithObject:object
                                   updateCallback:updateCallback];
  if (objectHandler.withCallback) {
    [objectHandler apply_updateCallback:context];
    return objectHandler;
  } else {
    return nil;
  }
}

@interface FunPtrManipulatedContent : NSObject
@property int y;
@end

@implementation FunPtrManipulatedContent {
  int _x;
}

- (instancetype)init {
  self = [super init];
  if (self != nil) {
    _y = 0;
  }
  return self;
}

void call__updateContent(FunPtrManipulatedContent* manipulatedContent) {
  [manipulatedContent _updateContent];
}

+ (FunPtrManipulatedContent*)FunPtrManipulatedContentWithObject:
    (NSObject*)object {
  FunPtrManipulatedContent* manipulatedContent = [self new];
  id objectHandler = [manipulatedContent
      identity:FunPtrObjectHandlerForObject(
                   object, &call__updateContent, manipulatedContent)];
  if (objectHandler) {
    return manipulatedContent;
  } else {
    return nil;
  }
}

- (id)identity:(id)objectHandler {
  return objectHandler;
}

- (void)_updateContent {
  _x = 5;
  _y = 5;
}

- (int)get_x {
  return _x;
}

@end

void apply_second_funptr(NSObject* obj,
                         void (*funptr)(FunPtrExample*, NSObject*),
                         FunPtrExample* context) {
  (*funptr)(context, obj);
}

id ObjectForClass(Class klass);
void apply_funptr(void (*funptr)(FunPtrExample*), FunPtrExample* context) {
  funptr(context);
}

@implementation FunPtrExample

void call_update_manipulatedContent(FunPtrExample* context, NSObject* obj) {
  [context update_manipulatedContent:obj];
}

void call_apply_second_funptr(FunPtrExample* context, NSObject* obj) {
  apply_second_funptr(obj, &call_update_manipulatedContent, context);
}

void call_manipulationEntry_pass_obj(FunPtrExample* context) {
  FunPtrManipulationEntry* manipulationEntry = ObjectForClass( // skipped call
      [FunPtrManipulationEntry class]);
  [manipulationEntry pass_obj:&call_apply_second_funptr context:context];
}

- (void)application:(BOOL)condition {
  _manipulatedContent = nil;
  if (condition) {
    apply_funptr(&call_manipulationEntry_pass_obj, self);
  } else {
    _manipulatedContent = [FunPtrManipulatedContent new];
  }
}

- (void)update_manipulatedContent:(NSObject*)object {
  _manipulatedContent =
      [FunPtrManipulatedContent FunPtrManipulatedContentWithObject:object];
}

@end

BOOL test_FunPtrExample_YES_using_property_bad() {
  FunPtrExample* example = [FunPtrExample new];
  [example application:YES];
  int y = example.manipulatedContent.y;
  int* ptr = &y;
  if (y == 5) {
    ptr = NULL;
  }
  return *ptr == y;
}

BOOL test_FunPtrExample_YES_using_property_good() {
  FunPtrExample* example = [FunPtrExample new];
  [example application:YES];
  int y = example.manipulatedContent.y;
  int* ptr = &y;
  if (example.manipulatedContent && y != 5) {
    ptr = NULL;
  }
  return *ptr == y;
}

BOOL test_FunPtrExample_NO_using_property_good() {
  FunPtrExample* example = [FunPtrExample new];
  [example application:NO];
  int y = example.manipulatedContent.y;
  int* ptr = &y;
  if (y != 0) {
    ptr = NULL;
  }
  return *ptr == y;
}

BOOL test_FunPtrExample_YES_using_ivar_bad() {
  FunPtrExample* example = [FunPtrExample new];
  [example application:YES];
  int x = [example.manipulatedContent get_x];
  int* ptr = &x;
  if (x == 5) {
    ptr = NULL;
  }
  return *ptr == x;
}

BOOL test_FunPtrExample_YES_using_ivar_good() {
  FunPtrExample* example = [FunPtrExample new];
  [example application:YES];
  int x = [example.manipulatedContent get_x];
  int* ptr = &x;
  if (example.manipulatedContent && x != 5) {
    ptr = NULL;
  }
  return *ptr == x;
}

// ivars should be initialized to 0 by default
// https://developer.apple.com/documentation/objectivec/nsobject/1571958-alloc
// > memory for all other instance variables is set to 0
BOOL test_FunPtrExample_NO_using_ivar_good_FP() {
  FunPtrExample* example = [FunPtrExample new];
  [example application:NO];
  int x = [example.manipulatedContent get_x];
  int* ptr = &x;
  if (x != 0) {
    ptr = NULL;
  }
  return *ptr == x;
}
