/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import <Foundation/NSObject.h>

@interface AClass : NSObject
    @property (nonatomic, strong) NSObject* aDynValue;
@end


@implementation AClass

@dynamic aDynValue;

@end
