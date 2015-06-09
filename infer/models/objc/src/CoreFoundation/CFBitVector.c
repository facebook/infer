#import <Foundation/Foundation.h>

CFBitVectorRef __cf_alloc(CFBitVectorRef);
CFBitVectorRef __cf_non_null_alloc(CFBitVectorRef);

CFBitVectorRef CFBitVectorCreate ( CFAllocatorRef allocator,
                                   const UInt8 *bytes,
                                   CFIndex numBits ) {
  CFBitVectorRef c;
  return __cf_non_null_alloc(c);
}
