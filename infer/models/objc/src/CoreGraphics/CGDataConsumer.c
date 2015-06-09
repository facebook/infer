#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGDataConsumerRef);

void CGDataConsumerRelease ( CGDataConsumerRef consumer ) {
    if (consumer) __objc_release_cf(consumer);
}

