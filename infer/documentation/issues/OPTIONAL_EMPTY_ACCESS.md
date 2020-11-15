Optional Empty Access warnings are reported when we try to retrieve the value of a [`folly::Optional`](https://github.com/facebook/folly/blob/master/folly/Optional.h) when it is empty (i.e. `folly::none`).

In the following example we get a warning as `int_opt` might be `folly::none` and its value is being accessed:

```cpp
bool somef(int v);

folly::Optional<int> mightReturnNone(int v) {
   if (somef(v)) {
      return folly::Optional(v);
   }

   return folly::none;
}

int value_no_check() {
  folly::Optional<int> int_opt = mightReturnNone (4);
  return int_opt.value(); // Optional Empty Access warning
}
```

We do not get the warning anymore if we add a check whether `int_opt` is not empty:

```cpp
int value_check() {
  folly::Optional<int> int_opt = mightReturnNone (4);
  if (int_opt.has_value()) {
     return int_opt.value(); // OK
  }
  return -1;
}
```

In some cases we know that we have a non-empty value and there is no need to have a check. Consider the following example where Infer does not warn:

```cpp
bool somef(int v) {return v > 3;};

folly::Optional<int> mightReturnNone(int v) {
   if (somef(v)) {
      return folly::Optional(v);
   }

   return folly::none;
}

int value_no_check() {
  folly::Optional<int> int_opt = mightReturnNone (4); // cannot be folly::none
  return int_opt.value(); // OK
}
```
