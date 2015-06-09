#import <Foundation/Foundation.h>
#import <CoreText/CoreText.h>

CTParagraphStyleRef __cf_alloc(CTParagraphStyleRef);
CTParagraphStyleRef __cf_non_null_alloc(CTParagraphStyleRef);


CTParagraphStyleRef CTParagraphStyleCreate ( const CTParagraphStyleSetting *settings,
                                             size_t settingCount ) {
    CTParagraphStyleRef c;
    return __cf_alloc(c);
}
