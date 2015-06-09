#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGPDFScannerRef);

void CGPDFScannerRelease ( CGPDFScannerRef scanner ) {
    if (scanner) __objc_release_cf(scanner);
}

