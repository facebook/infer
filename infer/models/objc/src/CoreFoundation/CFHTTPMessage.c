#import <Foundation/Foundation.h>

CFHTTPMessageRef __cf_alloc(CFHTTPMessageRef);


CFHTTPMessageRef CFHTTPMessageCreateCopy ( CFAllocatorRef alloc,
                                           CFHTTPMessageRef message ) {
    CFHTTPMessageRef c;
    return __cf_alloc(c);
}

CFHTTPMessageRef CFHTTPMessageCreateEmpty ( CFAllocatorRef alloc,
                                            Boolean isRequest ) {
    CFHTTPMessageRef c;
    return __cf_alloc(c);
}

CFHTTPMessageRef CFHTTPMessageCreateRequest ( CFAllocatorRef alloc,
                                              CFStringRef requestMethod,
                                              CFURLRef url,
                                              CFStringRef httpVersion ) {
    CFHTTPMessageRef c;
    return __cf_alloc(c);
}

CFHTTPMessageRef CFHTTPMessageCreateResponse ( CFAllocatorRef alloc,
                                               CFIndex statusCode,
                                               CFStringRef statusDescription,
                                               CFStringRef httpVersion ) {
    CFHTTPMessageRef c;
    return __cf_alloc(c);
}
