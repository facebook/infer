#import <Foundation/Foundation.h>

CFRunLoopSourceRef __cf_alloc(CFRunLoopSourceRef);
CFRunLoopSourceRef __cf_non_null_alloc(CFRunLoopSourceRef);

CFRunLoopSourceRef CFRunLoopSourceCreate ( CFAllocatorRef allocator,
                                           CFIndex order,
                                           CFRunLoopSourceContext *context ) {
   CFRunLoopSourceRef c;
   return __cf_non_null_alloc(c);
}

CFRunLoopSourceRef CFSocketCreateRunLoopSource ( CFAllocatorRef allocator,
                                                 CFSocketRef s,
                                                 CFIndex order ) {
   CFRunLoopSourceRef c;
   return __cf_non_null_alloc(c);
}
