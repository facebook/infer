#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGFunctionRef);

void CGFunctionRelease ( CGFunctionRef function ) {
    if (function) __objc_release_cf(function);
}

