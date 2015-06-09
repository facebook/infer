#import <Foundation/Foundation.h>

CFLocaleRef __cf_alloc(CFLocaleRef);
CFLocaleRef __cf_non_null_alloc(CFLocaleRef);

CFLocaleRef CFLocaleCopyCurrent(void) {
  CFLocaleRef c;
  return __cf_non_null_alloc(c);
}

CFLocaleRef CFLocaleCreate ( CFAllocatorRef allocator,
                             CFStringRef localeIdentifier ) {
  CFLocaleRef c;
  return __cf_alloc(c);
}
