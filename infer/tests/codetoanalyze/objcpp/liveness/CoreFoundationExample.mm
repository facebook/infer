/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

static NSDictionary* dictionaryRepresentationFromCFPreferences(
    NSString* preferencesID) {
  CFStringRef ID = (__bridge CFStringRef)preferencesID;
  return (__bridge_transfer NSDictionary*)CFPreferencesCopyMultiple(
      NULL, ID, kCFPreferencesCurrentUser, kCFPreferencesAnyHost);
}

// mock for testing
class CKComponentScope {
 public:
  CKComponentScope(Class __unsafe_unretained componentClass,
                   id identifier = nil,
                   id (^initialStateCreator)(void) = nil);
  ~CKComponentScope();
  int a;

 private:
  CKComponentScope(const CKComponentScope&) = delete;
  CKComponentScope& operator=(const CKComponentScope&) = delete;
};

@interface ScopeTest : NSObject
@end

@implementation ScopeTest

+ (void)dead_ckcomponentscope_ok {
  CKComponentScope scope(self); // created for side effects; should not warn
}

@end
