This check flags when the result of a call to the method `cStringUsingEncoding` of `NSString` or its property `UTF8String`
is captured in an escaping block. In both cases this C string is a pointer to a structure inside the string object, which
may have a lifetime shorter than the string object and will certainly not have a longer lifetime. Therefore, you should copy
the C string if it needs to be stored outside of the memory context in which you use it.

Example:

```
  char* encoding = (char*)[name cStringUsingEncoding:NSUTF8StringEncoding];
  dispatch_async(dispatch_get_main_queue(), ^{
    const char* c = encoding; //bug
  });



  char* utf8string = name.UTF8String;
  dispatch_async(dispatch_get_main_queue(), ^{
    const char* c = utf8string; //bug
  });

```

This could cause crashes because the variable is likely to be freed when the code is executed, leaving the pointer dangling.
