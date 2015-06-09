#import <Foundation/Foundation.h>

CFMutableArrayRef __cf_alloc(CFMutableArrayRef);
CFMutableArrayRef __cf_non_null_alloc(CFMutableArrayRef);

CFMutableArrayRef CFArrayCreateMutable ( CFAllocatorRef allocator,
                                         CFIndex capacity,
                                         const CFArrayCallBacks *callBacks ) {
  CFMutableArrayRef c;
  return __cf_alloc(c);
}
