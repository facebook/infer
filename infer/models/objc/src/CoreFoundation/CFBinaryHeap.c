#import <Foundation/Foundation.h>

CFBinaryHeapRef __cf_alloc(CFBinaryHeapRef);


CFBinaryHeapRef CFBinaryHeapCreate ( CFAllocatorRef allocator,
                                     CFIndex capacity,
                                     const CFBinaryHeapCallBacks *callBacks,
                                     const CFBinaryHeapCompareContext *compareContext) {
  CFBinaryHeapRef c;
  return __cf_alloc(c);
}
