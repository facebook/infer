#import <Foundation/Foundation.h>

CFUUIDRef __cf_alloc(CFUUIDRef);
CFUUIDRef __cf_non_null_alloc(CFUUIDRef);

CFUUIDRef CFUUIDCreate ( CFAllocatorRef alloc ){
    CFUUIDRef c;
    return __cf_non_null_alloc(c);
}

CFUUIDRef CFUUIDCreateFromUUIDBytes ( CFAllocatorRef alloc, CFUUIDBytes bytes ) {

    CFUUIDRef c;
    return __cf_non_null_alloc(c);
}
