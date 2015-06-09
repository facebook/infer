#import <Foundation/Foundation.h>
#import <stdlib.h>

CFStringRef __cf_alloc(CFStringRef);

void __get_array_size(const UInt8);

CFStringRef CFStringCreateWithBytesNoCopy (
                        CFAllocatorRef alloc,
                        const UInt8 *bytes,
                        CFIndex numBytes,
                        CFStringEncoding encoding,
                        Boolean isExternalRepresentation,
                        CFAllocatorRef contentsDeallocator ) {
   CFStringRef c;
   CFStringRef s =  __cf_alloc(c);
   if (s) {
     if (bytes) {
         __get_array_size(bytes);
         free(bytes);
     }
   }
   return s;
}
