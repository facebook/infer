/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>
#import <Foundation/NSBundle.h>

@interface B : NSObject

- (BOOL)isC:(Class)aClass;

@end

@implementation B : NSObject

+ (void)b_m {
}

- (BOOL)isC:(Class)aClass {
  return TRUE;
}

@end

@interface C : NSObject

- (void)test;

+ (void)test_class;

@end

@interface A : C

@end

@implementation A

- (void)test {
}

- (void)call_test {
  [self test];
}

+ (void)test_class {
}

+ (void)call_test_class {
  [self test_class];
}

+ (void)call_alloc_class {
  [self alloc];
}

- (void)call_alloc_instance {
  [[self class] alloc];
}

- (void)call_class_instance {
  [[self class] test_class];
}

- (void)call_class_instance_with_class_name {
  [A test_class];
}

- (void)t {
  B* b = [B new];
  [[b class] b_m];
}

- (BOOL)use_class_in_other_ways:(B*)object {
  return [object isC:[self class]];
}

+ (void)calling_super {
  [super test_class];
}

- (void)init {
  [super init];
}

- (NSString*)loggerName {
  return NSStringFromClass([self class]);
}

+ (int)used_in_binary_op:(Class)c {
  if (self != c) {
    return 1;
  } else
    return 0;
}

Class foo();

BOOL class_method_in_conditional() {
  Class c = foo();
  if ([c class]) {
    return YES;
  }
  return NO;
}

- (NSBundle*)class_method_fst_arg_of_class_method {
  return [NSBundle bundleForClass:[A class]];
}

+ (NSBundle*)class_method_fst_arg_of_class_method_inside_instance_method {
  static NSBundle* bundle;
  NSString* stringsBundlePath =
      [[NSBundle bundleForClass:[B class]] pathForResource:@"Strings"
                                                    ofType:@"bundle"];
  bundle = [NSBundle bundleWithPath:stringsBundlePath] ?: [NSBundle mainBundle];
  return bundle;
}

@end
