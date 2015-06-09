#import <CoreText/CTFont.h>

CTFontRef __cf_alloc(CTFontRef);

CTFontRef CTFontCreateWithName ( CFStringRef name,
                                 CGFloat size,
                                 const CGAffineTransform *matrix ) {
  CTFontRef c;
  return __cf_alloc(c);
}
