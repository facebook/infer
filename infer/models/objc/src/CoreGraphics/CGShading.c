#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGShadingRef);

void CGShadingRelease ( CGShadingRef shading ) {
    if (shading) __objc_release_cf(shading);
}

