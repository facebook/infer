//
//  Hello.m
//  HelloWorldApp
//
//  Created by Dulma Rodriguez on 20/05/2015.
//  Copyright (c) 2015 Dulma Rodriguez. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Hello.h"

@implementation Hello

-(Hello*) return_hello {
    return [Hello new];
}

-(NSString*) null_dereference_bug {
    Hello *hello = nil;
    return hello->_s;
}

-(NSString*) ivar_not_nullable_bug:(Hello*) hello {
    Hello* ret_hello = [hello->_hello return_hello];
    return ret_hello->_s;
}

-(NSString*) parameter_not_null_checked_bug:(Hello*) hello {
    Hello *ret_hello = [hello return_hello];
    return ret_hello->_s;
}

@end
