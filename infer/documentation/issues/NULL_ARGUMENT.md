This issue type indicates `nil` or `null` being passed as argument where a non-null value is expected.

With `--pulse-nullability-annotations`, this is also reported when null is passed to a C function or
C++ method without a known implementation whose declaration marks the parameter `_Nonnull` or
`__attribute__((nonnull))`:

```c
#include <stdlib.h>

int parse_level(const char* _Nonnull s);

int level_from_environment(void) {
  // ERROR: getenv() returns NULL when LEVEL is not set
  return parse_level(getenv("LEVEL"));
}
```

Argument positions are counted from 1 and include the implicit object parameter of C++ methods, as in
`nonnull(i)`. Variadic arguments are not checked.

In Objective-C, some methods are known to crash when passed `nil`:

```objc
#import <Foundation/Foundation.h>

NSString* stringNotNil(NSString* str) {
  if (!str) {
    // ERROR: NSString:stringWithString: expects a non-nil value
    return [NSString stringWithString:nil];
  }
  return str;
}
```
