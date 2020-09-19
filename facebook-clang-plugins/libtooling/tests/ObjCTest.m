/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "FoundationStub.h"

@protocol MyProtocol
@property(nonatomic, copy) NSString *str;
@end

@protocol SomeProtocol;

@interface MyClass : NSObject<MyProtocol>

@property(nonatomic, copy) NSString *str;

@property(nonatomic, assign) void *x;
@property(nonatomic, assign) int y;
@property(nonatomic, assign) NSObject<SomeProtocol> *delegate;

@end

@interface MyClass ()

- (void)foo:(NSString *__nonnull)s;

@end

@implementation MyClass

- (void)foo:(NSString *__nonnull)s {

  NSLog(@"%s\n", @encode(int **));

  NSLog(@"%d\n", [self respondsToSelector:@selector(foo:)]);

  NSLog(@"%d\n", [[self class] conformsToProtocol:@protocol(MyProtocol)]);

  NSUInteger (^block)(NSString *x) = ^(NSString *x) {
    self.str = x;
    return [x length];
  };

  @try {
    NSArray *a = @[ @YES ];
    NSLog(@"%@\n", a[0]);

    NSDictionary *d = @{@"key" : @1};
    NSLog(@"%@\n", d[@"wrong key"]);
  } @catch (NSException *e) {
    NSLog(@"Exception: %@", e);
  } @finally {
    NSLog(@"finally");
  }

  goto theend;
  return;
theend:
  NSLog(@"jumped");
}

@end

@interface MyClass (MyCategory)

- (void)bar:(NSString *)s;

@end

@implementation MyClass (MyCategory)

- (void)bar:(NSString *)s {
  self.x = NULL;
  self.y = 0;
  self.delegate = nil;
}

@end

@interface MyClassGenerics<ObjectType> : NSObject

@end

@interface BarGenerics : NSObject
+ (instancetype)newWithCs:(MyClassGenerics<NSObject *> *)Cs;
@end

@interface MySubclass : MyClass
@end

@implementation MySubclass

- (void)bar:(NSString *)s {
  [super bar:s];
}

- (void)foo:(NSString *__nonnull)s {
  [super foo:s];
}

@end

int main(int argc, char **argv) {
  @autoreleasepool {
    [[[MyClass alloc] init] foo:@"hello"];
    [[MyClass new] bar:@"hello"];
  }
  return 0;
}
