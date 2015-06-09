/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

@interface B : NSObject

@end

@implementation B

- (instancetype) init
{
    return nil;
}

@end

@interface A : B

@end

@implementation A
{
    int a;
}

- (instancetype) init
{
    self = [super init];
    self->a = 4;
    return self;
}

@end

int main(int argc, char *argv[]) {
    @autoreleasepool {
        __unused id a = [A new];
    }
}
