#import <Foundation/Foundation.h>

CFDateRef __cf_alloc(CFDateRef);
CFDateRef __cf_non_null_alloc(CFDateRef);


CFDateRef CFDateCreate ( CFAllocatorRef allocator, CFAbsoluteTime at ) {
  CFDateRef c;
  return __cf_non_null_alloc(c);
}
