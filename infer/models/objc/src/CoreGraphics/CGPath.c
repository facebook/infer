#import <CoreGraphics/CoreGraphics.h>
#import <Foundation/Foundation.h>

CGPathRef __cf_non_null_alloc(CGPathRef);
void __objc_release_cf(CGPathRef);

CGMutablePathRef CGPathCreateMutable () {
    CGMutablePathRef c;
    return (CGMutablePathRef) __cf_non_null_alloc(c);
}

CGPathRef CGPathCreateWithRect ( CGRect rect, const CGAffineTransform *transform ) {
    CGPathRef c;
    return __cf_non_null_alloc(c);
}

void CGPathRelease ( CGPathRef path ) {
    if (path) __objc_release_cf(path);
}


CGPathRef CGPathCreateWithEllipseInRect ( CGRect rect,
                                          const CGAffineTransform *transform ){
    CGPathRef c;
    return __cf_non_null_alloc(c);
}

CGPathRef CGPathCreateCopy ( CGPathRef path ) {
    CGPathRef c;
    return __cf_non_null_alloc(c);
}

CGPathRef CGPathCreateCopyByTransformingPath ( CGPathRef path,
                                               const CGAffineTransform *transform ) {
    CGPathRef c;
    return __cf_non_null_alloc(c);
}

CGPathRef CGPathCreateWithRoundedRect ( CGRect rect,
                                        CGFloat cornerWidth,
                                        CGFloat cornerHeight,
                                        const CGAffineTransform *transform) {
    CGPathRef c;
    return __cf_non_null_alloc(c);
}
