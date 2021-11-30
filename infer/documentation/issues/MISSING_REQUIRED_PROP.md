This issues is reported when a required `@Prop` is missing.


## Examples

Assume that the following Litho Component specification is defined as follows where `prop1` is optional and `prop2` is required.

```java
class MyComponentSpec {

  static void onCreate(
      ComponentContext c,
      @Prop(optional = true) String prop1, @Prop int prop2) {
    ...
  }
  ...
}
```

When we build the corresponding component, we should have all the required props. If we are missing optional props (e..g `prop1` below), it is ok.

```java
MyComponent.create(c)
    .prop2(8)
    .build();
```

However, if we are missing a required prop, Infer gives an error below for the missing `prop2`.

```java
MyComponent.create(c)
    .prop1("My prop 1")
    .build();
```

** Action **

There are two ways to fix this issue.

First, we could add the missing `prop2`:

```java
MyComponent.create(c)
    .prop1("My prop 1")
    .prop2(x) // where x is some integer
    .build();
```

or alternatively, if the `prop2` is not really required, we could change the component spec to reflect that:


```java
class MyComponentSpec {

  static void onCreate(
      ComponentContext c,
      @Prop(optional = true) String prop1, @Prop(optional = true) int prop2) {
    ...
  }
  ...
}
```