#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGLayerRef);

void CGLayerRelease ( CGLayerRef layer ){
    if (layer) __objc_release_cf(layer);
}

