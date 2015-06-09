/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

@interface MyClass : NSObject
+ (void)aClassMethod;
- (void)anInstanceMethod;
+ (void)aClassMethod2;
- (int) getX;
@end

@implementation MyClass 
+ (void)aClassMethod {
    MyClass *myClass = [self alloc];
}

- (void)anInstanceMethod {
    [MyClass aClassMethod];
}

+ (void)aClassMethod2 {
    [self aClassMethod];
}

- (int) getX {
    return 0;
}

- (void)anInstanceMethod2 {
    [self getX];
}
@end
