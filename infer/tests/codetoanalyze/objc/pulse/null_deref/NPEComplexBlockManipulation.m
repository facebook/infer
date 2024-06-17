/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@class ManipulatedContent;

@interface Example : NSObject

@property ManipulatedContent* manipulatedContent;

@end

@interface ManipulationEntry : NSObject

@end

@implementation ManipulationEntry {
  NSObject* _obj;
}

- (void)pass_obj:(void (^)(NSObject*))completion {
  completion(_obj);
}

@end

@interface WithCallback : NSObject

- (void)apply_callback:(void (^)())callback;

@end

@implementation WithCallback

- (void)apply_callback:(void (^)())callback {
  callback();
}

@end

@interface ObjectHandler : NSObject {
  NSObject* _object;
  void (^_updateCallback)(void);
}

@property WithCallback* withCallback;

- (instancetype)initWithObject:(NSObject*)object
                updateCallback:(void (^)(void))updateCallback;

@end

@implementation ObjectHandler

- (instancetype)initWithObject:(NSObject*)object
                updateCallback:(void (^)(void))updateCallback {
  if (self = [super init]) {
    _object = object;
    _updateCallback = updateCallback;
  }
  return self;
}

- (void)apply_updateCallback {
  WithCallback* withCallback = _withCallback;
  __weak __typeof(self) weakSelf = self;
  [_withCallback apply_callback:^{ // specialized in pre-analysis
    __typeof(self) strongSelf = weakSelf;
    if (strongSelf) {
      strongSelf->_updateCallback(); // unknown call until we specialize
                                     // over captured variables
    }
  }];
}

@end

id ObjectHandlerForObject(NSObject* object, void (^updateCallback)(void)) {
  ObjectHandler* objectHandler =
      // no specialization on init methods
      [[ObjectHandler alloc] initWithObject:object
                             updateCallback:updateCallback];
  if (objectHandler.withCallback) {
    [objectHandler apply_updateCallback];
    return objectHandler;
  } else {
    return nil;
  }
}

@interface ManipulatedContent : NSObject
@property int y;
@end

@implementation ManipulatedContent {
  int _x;
}

- (instancetype)init {
  self = [super init];
  if (self != nil) {
    _y = 0;
  }
  return self;
}

+ (ManipulatedContent*)ManipulatedContentWithObject:(NSObject*)object {
  ManipulatedContent* manipulatedContent = [self new];
  id objectHandler = [manipulatedContent
      identity:ObjectHandlerForObject(object, ^{ // specialized in pre-analysis
        [manipulatedContent _updateContent];
      })];
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

void apply_second(NSObject* object, void (^block)(void)) { block(); }

id ObjectForClass(Class klass);
void apply(void (^block)(void)) { block(); }

@implementation Example

- (void)application:(BOOL)condition {
  _manipulatedContent = nil;
  if (condition) {
    apply(^{
      ManipulationEntry* manipulationEntry = ObjectForClass( // skipped call
          [ManipulationEntry class]);
      [manipulationEntry
          pass_obj:^(NSObject* obj) { // specialized in pre-analysis
            apply_second(obj, ^{ // specialized in pre-analysis
              // confusion in self: we expect it to be the one of the
              // current declaring context (an Example) but it is the
              // one of the call context (a MSYSSampleMailboxSessionBind)
              [self update_manipulatedContent:obj];
            });
          }];
    });
  } else {
    _manipulatedContent = [ManipulatedContent new];
  }
}

- (void)update_manipulatedContent:(NSObject*)object {
  _manipulatedContent =
      [ManipulatedContent ManipulatedContentWithObject:object];
}

@end

BOOL test_Example_YES_using_property_bad() {
  Example* example = [Example new];
  [example application:YES];
  int y = example.manipulatedContent.y;
  int* ptr = &y;
  if (y == 5) {
    ptr = NULL;
  }
  return *ptr == y;
}

BOOL test_Example_YES_using_property_good() {
  Example* example = [Example new];
  [example application:YES];
  int y = example.manipulatedContent.y;
  int* ptr = &y;
  if (example.manipulatedContent && y != 5) {
    ptr = NULL;
  }
  return *ptr == y;
}

BOOL test_Example_NO_using_property_good() {
  Example* example = [Example new];
  [example application:NO];
  int y = example.manipulatedContent.y;
  int* ptr = &y;
  if (y != 0) {
    ptr = NULL;
  }
  return *ptr == y;
}

BOOL test_Example_YES_using_ivar_bad() {
  Example* example = [Example new];
  [example application:YES];
  int x = [example.manipulatedContent get_x];
  int* ptr = &x;
  if (x == 5) {
    ptr = NULL;
  }
  return *ptr == x;
}

BOOL test_Example_YES_using_ivar_good() {
  Example* example = [Example new];
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
BOOL test_Example_NO_using_ivar_good_FP() {
  Example* example = [Example new];
  [example application:NO];
  int x = [example.manipulatedContent get_x];
  int* ptr = &x;
  if (x != 0) {
    ptr = NULL;
  }
  return *ptr == x;
}
