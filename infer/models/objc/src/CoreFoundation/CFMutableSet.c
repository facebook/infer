#import <Foundation/Foundation.h>

CFMutableSetRef __cf_alloc(CFMutableSetRef);
CFMutableSetRef __cf_non_null_alloc(CFMutableSetRef);

CFMutableSetRef CFSetCreateMutable (CFAllocatorRef allocator,
                                    CFIndex capacity,
                                    const CFSetCallBacks *callBacks) {
  CFMutableSetRef c;
  return __cf_alloc(c);
}

CFMutableSetRef CFSetCreateMutableCopy ( CFAllocatorRef allocator,
                                         CFIndex capacity,
                                         CFSetRef theSet ) {
  CFMutableSetRef c;
  return __cf_non_null_alloc(c);
}
