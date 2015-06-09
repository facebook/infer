#import <Foundation/Foundation.h>

CFNumberRef __cf_alloc(CFNumberRef);


CFNumberRef CFNumberCreate ( CFAllocatorRef allocator,
                             CFNumberType theType,
                             const void *valuePtr ) {
    CFNumberRef c;
    return __cf_alloc(c);
}
