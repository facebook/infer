#import <Foundation/Foundation.h>

CFStringTokenizerRef __cf_alloc(CFStringTokenizerRef);
CFStringTokenizerRef __cf_non_null_alloc(CFStringTokenizerRef);

CFStringTokenizerRef CFStringTokenizerCreate ( CFAllocatorRef alloc,
                                               CFStringRef string,
                                               CFRange range,
                                               CFOptionFlags options,
                                               CFLocaleRef locale ) {
    CFStringTokenizerRef c;
    return __cf_non_null_alloc(c);
}
