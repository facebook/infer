`Awaitable` values created by calls to asynchronous methods should eventually be `await`ed along all codepaths (even if their value is unused). Hence the following is *not* OK

```hack
class A {
  public static async genInt() : Awaitable<int>{
    // typically do something involving IO
  }

  public static async genBad() : Awaitable<void> {
    $_unused = self::genInt(); // ERROR: should have done $_unused = await self::genInt();
    return;
  }
}
```

Failure to `await` an `Awaitable` can lead to non-deterministic amount of the asynchronous call actually being executed, and can also indicate a logical confusion between `T` and `Awaitable<T>` that may not be caught by the type-checker.
