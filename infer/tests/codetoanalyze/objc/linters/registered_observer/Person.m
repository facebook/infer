/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

// OkPerson is the only class in the whole hierarchy that invokes removeObserver

@interface OkPerson : NSObject

@property(strong) NSNotificationCenter* nc;

@end

@implementation OkPerson

- (void)boo {
  [self.nc removeObserver:self];
}

@end

// ---

@interface OkPerson2 : OkPerson

@end

@implementation OkPerson2

- (void)foo:(NSMutableDictionary*)dict {
  self.nc = [NSNotificationCenter defaultCenter];
  [self.nc addObserver:self selector:@selector(foo:) name:nil object:nil];
}

@end

// ---

@interface OkPerson3 : OkPerson2

@end

@implementation OkPerson3

- (void)foo:(NSMutableDictionary*)dict {
  self.nc = [NSNotificationCenter defaultCenter];
  [self.nc addObserver:self selector:@selector(foo:) name:nil object:nil];
}

@end

// ---

// No one in the whole hierarchy of BadPerson invokes removeObserver

@interface BadPerson : NSObject

@property(strong) NSNotificationCenter* nc;

@end

@implementation BadPerson

@end

// ---

@interface BadPerson2 : BadPerson

@property(strong) NSNotificationCenter* nc;

@end

@implementation BadPerson2

- (void)foo:(NSMutableDictionary*)dict {
  self.nc = [NSNotificationCenter defaultCenter];
  [self.nc addObserver:self selector:@selector(foo:) name:nil object:nil];
}

@end

// ---

@interface BadPerson3 : BadPerson2

@property(strong) NSNotificationCenter* nc;

@end

@implementation BadPerson3

- (void)fooRegister:(NSMutableDictionary*)dict {
  self.nc = [NSNotificationCenter defaultCenter];
  [self.nc addObserver:self selector:@selector(foo:) name:nil object:nil];
}

@end
