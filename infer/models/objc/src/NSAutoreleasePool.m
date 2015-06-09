 /*
  * Copyright (c) 2014- Facebook.
  * All rights reserved.
  */

 #pragma clang diagnostic ignored "-Wprotocol"

 #pragma clang diagnostic ignored "-Wincomplete-implementation"

 #pragma clang diagnostic ignored "-Wimplicit-function-declaration"

 #import <Foundation/NSObject.h>

 @implementation NSAutoreleasePool

 - (id)init {
     return (id)[self autorelease];
 }

 @end
