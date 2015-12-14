/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

typedef  void (^MyHandler) (NSString *name);

@interface C : NSObject

@property (nonatomic, strong) MyHandler handler;

@property (nonatomic, strong) NSString *name;
@property (nonatomic,strong) C *bla1;
@property (nonatomic,strong) C *bla2;
@end

@implementation C

- (void) foo {
    static dispatch_once_t once;
    static C* sharedInstance1;
    dispatch_once(&once, ^{
        sharedInstance1 = [[C alloc] init];
    });
    static C* sharedInstance2;
    dispatch_once(&once, ^{
        sharedInstance2 = [[C alloc] init];
    });

     _bla1 = sharedInstance1;
     _bla2 = sharedInstance2;
    sharedInstance2.handler = ^(NSString* s){
        _name = sharedInstance2.name;
    };
}

@end

int main() {
    C *c = [[C alloc] init];
    [c foo];
    return 0;
}
