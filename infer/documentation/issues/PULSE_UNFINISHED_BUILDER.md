Classes adhering to builder pattern are usually expected to call a finalizer function at some point to produce final result based on values that were passed to a builder itself. If finalizer function hasn't been called then builder's data won't be consumed in any meaningful way and will just be discarded.

```hack
class MyBuilder {
  private int $a = 0;
  private int $b = 0;

  public function setA(int $a): MyBuilder {
    $this->a = $a;
    return $this;
  }

  public function setB(int $b): MyBuilder {
    $this->b = $b;
    return $this;
  }

  public function saveX(): Awaitable<void> {
    // typically do something involving IO
  }
}

class BuilderTester {
  public static function builderUserOK(): void {
    $b = new MyBuilder(0);
    $b->setA(42)->setB(97)->saveX();
  }

  public static function builderUserBad(): void {
    $b = new MyBuilder(0);
    $b->setA(42)->setB(97); // ERROR: saveX hasn't been called so the builder's data is discarded
  }
}
```
