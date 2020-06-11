A retain cycle is a situation when object A retains object B, and object B
retains object A at the same time. Here is an example:

```objectivec
@class Child;
@interface Parent : NSObject {
    Child *child; // Instance variables are implicitly __strong
}
@end
@interface Child : NSObject {
    Parent *parent;
}
@end
```

You can fix a retain cycle in ARC by using \_\_weak variables or weak properties
for your "back links", i.e. links to direct or indirect parents in an object
hierarchy:

```objectivec
@class Child;
@interface Parent : NSObject {
    Child *child;
}
@end
@interface Child : NSObject {
    __weak Parent *parent;
}
@end
```
