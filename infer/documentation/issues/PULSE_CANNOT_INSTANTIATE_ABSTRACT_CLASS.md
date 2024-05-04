Instantiating an abstract class will lead to `Cannot instantiate abstract class` error.

```hack
abstract class AbstractClass1 {}

class ConcreteClass1 extends AbstractClass1 {}

public static function makeGeneric<T>(classname<T> $cls): void {
    new $cls();
}

<<__ConsistentConstruct>>
abstract class AbstractClass2 {

  public static function makeStatic(): void {
    new static();
  }
}

class ConcreteClass2 extends AbstractClass2 {}

public function badViaGeneric(): void {
    Main::makeGeneric(AbstractClass1::class); // ERROR!
}

public function goodViaGeneric(): void {
  Main::makeGeneric(ConcreteClass1::class);
}

public function badViaStatic(): void {
  AbstractClass2::makeStatic(); // ERROR!
}

public function goodViaStatic(): void {
  ConcreteClass2::makeStatic();
}
```
