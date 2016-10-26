/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#ifdef __APPLE__
#import <Foundation/Foundation.h>
#elif __linux__
// fake foundation enough so linux doesn't complain
#define nil 0
@interface NSObject
@end
@implementation NSObject
@end
@interface NSString : NSObject
@end
@implementation NSString
@end
#else
#error "Need either os x or linux"
#endif

// Mimic importing CKComponnet
@interface CKComponent : NSObject
@end

// Mimic importing CKCompositeComponnet
@interface CKCompositeComponent : CKComponent
+ (instancetype)newWithComponent:(CKComponent*)component;
@end

// Mimic importing CKLabelComponent
typedef struct { NSString* string; } LabelAttributes;

typedef struct { int thisStructIsEmpty; } ViewAttributes;

typedef struct { int thisStructIsEmpty; } CKSize;

// Mimic importing CKComponentScope

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
