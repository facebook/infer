```objc
This issue type indicates `nil` being passed as argument where a non-nil value expected.

#import <Foundation/Foundation.h>

// Test (non-nil) returned values of NSString methods against `nil`
NSString* stringNotNil(NSString* str) {
  if (!str) {
        // ERROR: NSString:stringWithString: expects a non-nil value
	return [NSString stringWithString:nil];
  }
  return str;
}
```
