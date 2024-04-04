namespace LateBindingAndTrait;

trait T {
  require extends Base;

  public static function get(this::Info $params): int {
    return static::lookup($params);
  }

  public static function run(this::Info $params): void {
    \Level1\taintSink(static::get($params));
  }
}

class Base {
  const type Info = shape('good' => int, 'bad' => int);

  public static function lookup(this::Info $params): int {
    return $params['bad'];
  }
}

class C extends Base {
  use T;

  public static function lookup(this::Info $params): int {
    return $params['good'];
  }

}

class D extends Base {
  use T;

}

class Main {
  public function good(): void {
    C::run(shape('bad' => \Level1\taintSource(), 'good' => 0));
  }

  public function bad(): void {
    D::run(shape('bad' => \Level1\taintSource(), 'good' => 0));
  }

}
