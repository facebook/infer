#import <Foundation/Foundation.h>

CFSocketRef __cf_alloc(CFSocketRef);

CFSocketRef CFSocketCreate ( CFAllocatorRef allocator,
                             SInt32 protocolFamily,
                             SInt32 socketType,
                             SInt32 protocol,
                             CFOptionFlags callBackTypes,
                             CFSocketCallBack callout,
                             const CFSocketContext *context ) {
    CFSocketRef c;
    return __cf_alloc(c);
}
