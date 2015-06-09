#import <Foundation/Foundation.h>

CFRunLoopObserverRef __cf_alloc(CFRunLoopObserverRef);
CFRunLoopSourceRef __cf_non_null_alloc(CFRunLoopSourceRef);

CFRunLoopObserverRef CFRunLoopObserverCreate (  CFAllocatorRef allocator,
                                                CFOptionFlags activities,
                                                Boolean repeats,
                                                CFIndex order,
                                                CFRunLoopObserverCallBack callout,
                                                CFRunLoopObserverContext *context) {
    CFRunLoopObserverRef c;
    return __cf_non_null_alloc(c);
}


CFRunLoopObserverRef CFRunLoopObserverCreateWithHandler ( CFAllocatorRef allocator,
                                                          CFOptionFlags activities,
                                                          Boolean repeats,
                                                          CFIndex order,
                                                          void (^block)(
                                                          CFRunLoopObserverRef observer,
                                                          CFRunLoopActivity activity) ) {
    CFRunLoopObserverRef c;
    return __cf_non_null_alloc(c);
}
