/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
