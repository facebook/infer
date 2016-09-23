/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

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
