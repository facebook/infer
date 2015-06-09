#import <Foundation/Foundation.h>

CFMutableAttributedStringRef __cf_non_null_alloc(CFMutableAttributedStringRef);

CFMutableAttributedStringRef CFAttributedStringCreateMutable (CFAllocatorRef alloc,
                                                              CFIndex maxLength ) {
    CFMutableAttributedStringRef c;
    return __cf_non_null_alloc(c);
}
