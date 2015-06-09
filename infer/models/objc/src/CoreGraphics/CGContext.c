#import <CoreGraphics/CoreGraphics.h>
#import <ImageIO/ImageIO.h>
#import <QuartzCore/QuartzCore.h>

void __objc_release_cf(CGContextRef);

void CGContextRelease ( CGContextRef c ){
    if (c) __objc_release_cf(c);
}

CGContextRef __cf_alloc(CGContextRef);

CGContextRef CGBitmapContextCreate ( void *data,
                                     size_t width,
                                     size_t height,
                                     size_t bitsPerComponent,
                                     size_t bytesPerRow,
                                     CGColorSpaceRef space,
                                     CGBitmapInfo bitmapInfo ) {
    CGContextRef c;
    return __cf_alloc(c);
}
