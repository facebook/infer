/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

//
//  Hello.h
//  HelloWorldApp
//

#import <Foundation/Foundation.h>

@interface Hello : NSObject

@property(strong) NSString* s;
@property(strong) Hello* hello;

- (Hello*)return_hello;

- (NSString*)null_dereference_bug;

- (NSString*)ivar_not_nullable_bug:(Hello*)hello;

- (NSString*)parameter_not_null_checked_bug:(Hello*)hello;

@end
