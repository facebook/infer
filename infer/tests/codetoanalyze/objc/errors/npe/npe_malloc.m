/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>
#import <stdlib.h>

struct Person {
    int x;
    int y;
};

@interface C : NSObject

@end

@implementation C

- (struct Person*) test
{
    struct Person* person = (struct Person *)malloc(sizeof(struct Person));
    person->x = 10;
    return person;
}

@end
