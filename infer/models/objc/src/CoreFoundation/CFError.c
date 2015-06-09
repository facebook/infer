#import <Foundation/Foundation.h>

CFErrorRef __cf_alloc(CFErrorRef);
CFErrorRef __cf_non_null_alloc(CFErrorRef);


CFErrorRef CFReadStreamCopyError ( CFReadStreamRef stream ) {
  CFErrorRef c;
  return __cf_alloc(c);
}

CFErrorRef CFWriteStreamCopyError ( CFWriteStreamRef stream ) {
  CFErrorRef c;
  return __cf_alloc(c);
}
