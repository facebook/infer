/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

typedef void (^MyHandler)(NSString* name);

@interface RetainCSV : NSObject

@property(nonatomic, strong) MyHandler handler;

@property(nonatomic, strong) NSString* name;
@property(nonatomic, strong) RetainCSV* bla1;
@property(nonatomic, strong) RetainCSV* bla2;
@end

@implementation RetainCSV

- init {
  return self;
}

- (void)foo {
  static dispatch_once_t once;
  static RetainCSV* sharedInstance1;
  dispatch_once(&once, ^{
    sharedInstance1 = [[RetainCSV alloc] init];
  });
  static RetainCSV* sharedInstance2;
  dispatch_once(&once, ^{
    sharedInstance2 = [[RetainCSV alloc] init];
  });

  _bla1 = sharedInstance1;
  _bla2 = sharedInstance2;
  sharedInstance2.handler = ^(NSString* s) {
    _name = sharedInstance2.name;
  };
}

@end

int RetainCSVycleStaticVar() {
  RetainCSV* c = [[RetainCSV alloc] init];
  [c foo];
  return 0;
}
