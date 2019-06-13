/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void __free_cf(void* item);

void* CFAutorelease(void* item) { __free_cf(item); }

void* CFBridgingRelease(void* item) { __free_cf(item); }

void CGAffineTransformRelease(void* item) { __free_cf(item); }

void CGBaseRelease(void* item) { __free_cf(item); }

void CGBitmapContextRelease(void* item) { __free_cf(item); }

void CGColorRelease(void* item) { __free_cf(item); }

void CGColorSpaceRelease(void* item) { __free_cf(item); }

void CGContextRelease(void* item) { __free_cf(item); }

void CGDataConsumerRelease(void* item) { __free_cf(item); }

void CGDataProviderRelease(void* item) { __free_cf(item); }

void CGErrorRelease(void* item) { __free_cf(item); }

void CGFontRelease(void* item) { __free_cf(item); }

void CGFunctionRelease(void* item) { __free_cf(item); }

void CGGeometryRelease(void* item) { __free_cf(item); }

void CGGradientRelease(void* item) { __free_cf(item); }

void CGImageRelease(void* item) { __free_cf(item); }

void CGLayerRelease(void* item) { __free_cf(item); }

void CGPathRelease(void* item) { __free_cf(item); }

void CGPatternRelease(void* item) { __free_cf(item); }

void CGPDFArrayRelease(void* item) { __free_cf(item); }

void CGPDFContentStreamRelease(void* item) { __free_cf(item); }

void CGPDFContextRelease(void* item) { __free_cf(item); }

void CGPDFDictionaryRelease(void* item) { __free_cf(item); }

void CGPDFDocumentRelease(void* item) { __free_cf(item); }

void CGPDFObjectRelease(void* item) { __free_cf(item); }

void CGPDFOperatorTableRelease(void* item) { __free_cf(item); }

void CGPDFPageRelease(void* item) { __free_cf(item); }

void CGPDFScannerRelease(void* item) { __free_cf(item); }

void CGPDFStreamRelease(void* item) { __free_cf(item); }

void CGPDFStringRelease(void* item) { __free_cf(item); }

void CGShadingRelease(void* item) { __free_cf(item); }
