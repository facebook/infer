#import <CoreText/CTFramesetter.h>

CTFramesetterRef __cf_alloc(CTFramesetterRef);

CTFramesetterRef CTFramesetterCreateWithAttributedString ( CFAttributedStringRef string ) {
    CTFramesetterRef c;
    return __cf_alloc(c);
}
