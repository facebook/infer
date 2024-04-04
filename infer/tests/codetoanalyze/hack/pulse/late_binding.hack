namespace LateBinding;

class A {
  public static function caller1(): int {
    return static::call_with_late_binding();
  }

  public static function call_with_late_binding(): int {
    return 1;
  }
}

class B extends A {

  public static function caller2(): int {
    return static::call_with_late_binding();
  }

}

class C extends B {
  public static function parent_caller(): int {
    return parent::caller1();
  }

  public static function caller1(): int {
    return 3;
  }
  public static function call_with_late_binding(): int {
    return 0;
  }
}

class Main {

  public function call_caller_bad(): void {
    $tainted = \Level1\taintSource();
    $i = C::caller2();
    if ($i === 0) {
      \Level1\taintSink($tainted);
    }
  }

  public function call_caller_good(): void {
    $tainted = \Level1\taintSource();
    $i = C::caller2();
    if ($i != 0) {
      \Level1\taintSink($tainted);
    }
  }

  public function call_parent_caller_bad(): void {
    $tainted = \Level1\taintSource();
    $i = C::parent_caller();
    if ($i == 0) {
      \Level1\taintSink($tainted);
    }
  }

  public function call_parent_caller_good(): void {
    $tainted = \Level1\taintSource();
    $i = C::parent_caller();
    if ($i != 0) {
      \Level1\taintSink($tainted);
    }
  }

  public static function call_with_classname(classname<A> $cn): int {
    return $cn::call_with_late_binding();
  }

  public function call_C_with_classname_bad(): void {
    $tainted = \Level1\taintSource();
    $i = self::call_with_classname(C::class);
    if ($i == 0) {
      \Level1\taintSink($tainted);
    }
  }

  public function call_C_with_classname_good(): void {
    $tainted = \Level1\taintSource();
    $i = self::call_with_classname(C::class);
    if ($i != 0) {
      \Level1\taintSink($tainted);
    }
  }

  public function call_A_with_classname_bad(): void {
    $tainted = \Level1\taintSource();
    $i = self::call_with_classname(A::class);
    if ($i == 1) {
      \Level1\taintSink($tainted);
    }
  }

  public function call_A_with_classname_good(): void {
    $tainted = \Level1\taintSource();
    $i = self::call_with_classname(A::class);
    if ($i != 1) {
      \Level1\taintSink($tainted);
    }
  }

}
