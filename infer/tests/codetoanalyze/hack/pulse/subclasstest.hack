// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace SubClassTest;

class D {}
final class C extends D {}
class E extends D {}
class F extends E {}

interface I {}
interface J {}

class H implements I, J {}

class K<T> {}
class L<T> extends K<T> {}

class Wrapper {
  public async function fail(): Awaitable<int> {
    return 99;
  }

  public async function checkReflOK(): Awaitable<void> {
    $v = new C();
    if ($v is C) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkExtendsOK(): Awaitable<void> {
    $v = new C();
    if ($v is D) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkExtendsGenericOK(): Awaitable<void> {
    $v = new L<int>();
    if ($v is K<_>) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkExtendsGenericParamTestOK(
    L<int> $v,
  ): Awaitable<void> {
    if ($v is K<_>) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkTransOK(): Awaitable<void> {
    $v = new F();
    if ($v is D) {
      return;
    }
    $_ = $this->fail();
  }

  public async function checkTransBad(): Awaitable<void> {
    $v = new F();
    if ($v is D) {
      $_ = $this->fail();
    }
    return;
  }

  // this is designed to test preservation of instanceof facts
  // works fine because we do add_term_eq and then merge_vars
  // does the right thing
  public async function preserveTypeTestOK(mixed $m): Awaitable<void> {
    if ($m is D) {
      if ($m is D) {
        return;
      } else {
        $_ = $this->fail();
      }
    } else {
      return;
    }
  }

  // C and E are incomparable
  public async function belowFactPreservedOK(mixed $m): Awaitable<void> {
    if ($m is C) {
      if ($m is E) {
        $_ = $this->fail();
        // can't happen because $m can't be C and E at the same time
        // note that that depends on the concrete/abstract types form a tree
        // rather than a more general DAG.
      }
    }
    return;
  }

  public async function incomparableOneNullableOK(mixed $m): Awaitable<void> {
    if ($m is C) {
      if ($m is ?E) {
        $_ = $this->fail();
        // still can't happen
      }
    }
    return;
  }

  public async function incomparableOneNullableOK2(mixed $m): Awaitable<void> {
    if ($m is ?C) {
      if ($m is E) {
        $_ = $this->fail();
        // as above with nullable in other position
      }
    }
    return;
  }

  public async function incomparableTwoNullablesBad(mixed $m): Awaitable<void> {
    if ($m is ?C) {
      if ($m is ?E) {
        $_ = $this->fail();
        // now $m could be null, so this is reachable
      }
    }
    return;
  }

  // I and J are incomparable interfaces
  public async function belowIncomparableInterfacesBad(
    mixed $m,
  ): Awaitable<void> {
    if ($m is I) {
      if ($m is J) {
        $_ = $this->fail();
        // only upwards-closed world assumption
        // there could be a concrete type that is both I and J
      }
    }
    return;
  }

  // C is final, doesn't implement I
  public async function finalClassKnowInterfacesOK(mixed $m): Awaitable<void> {
    if ($m is C) {
      if ($m is I) {
        $_ = $this->fail();
        // if it's less than C then it's equal to C and so can't be I
      }
    }
    return;
  }

  public async function finalClassKnowInterfacesOK2(mixed $m): Awaitable<void> {
    if ($m is I) {
      if ($m is C) {
        $_ = $this->fail();
        // same as above with tests in the other order
      }
    }
    return;
  }

  // D is not final, doesn't implement I
  public async function nonFinalClassDunnoInterfacesBad(
    mixed $m,
  ): Awaitable<void> {
    if ($m is D) {
      if ($m is I) {
        $_ = $this->fail();
        // if it's less than D then it *might* still be something that implements I
      }
    }
    return;
  }

  // D is a subtype of E
  public async function belowFactImplicationBad(mixed $m): Awaitable<void> {
    if ($m is E) {
      if ($m is D) {
        $_ = $this->fail();
        // always fails
      }
    }
    return;
  }

  public async function belowFactImplicationOK(mixed $m): Awaitable<void> {
    if ($m is E) {
      if ($m is D) { // always succeeds
        return;
      } else {
        $_ = $this->fail(); // never happens
      }
    }
    return;
  }

  public async function nullIsNonnullOK(): Awaitable<void> {
    $x = null;
    if ($x is D) {
      $_ = $this->fail(); // never happens
    }
    return;
  }

  public async function nullIsNonnullBad(): Awaitable<void> {
    $x = null;
    if ($x is D) {
      return; // never happens
    }
    $_ = $this->fail();
  }

  public async function nullIsNullableBad(): Awaitable<void> {
    $x = null;
    if ($x is ?D) {
      $_ = $this->fail(); // always happens
    }
    return;
  }

  public async function nullIsNullableOK(): Awaitable<void> {
    $x = null;
    if ($x is ?D) {
      return; // always happens
    }
    $_ = $this->fail();
  }

  public async function objectIsNullableOK(): Awaitable<void> {
    $x = new D();
    if ($x is ?D) {
      return; // always happens
    }
    $_ = $this->fail();
  }

  // E subtype of D
  public async function assumeParameterTypeOK_FP(E $x): Awaitable<void> {
    if ($x is D) {
      return; // always happens
    }
    $_ = $this->fail();
  }

  public async function assumeParameterTypeGuardedOK_FP(E $x): Awaitable<void> {
    if ($x is null) {
      return;
    }
    if ($x is D) {
      return; // always happens
    }
    $_ = $this->fail();
  }

  public async function assumeNullableParameterTypeBad(?E $x): Awaitable<void> {
    if ($x is D) {
      return; // often happens
    }
    $_ = $this->fail(); // can happen if $x is null
  }

  public async function assumeNullableParameterTypeOK_FP(
    ?E $x,
  ): Awaitable<void> {
    if ($x is ?D) {
      return; // always
    }
    $_ = $this->fail();
  }

  // C incompatible with E
  public async function assumeParameterTypeOK2_FP(E $x): Awaitable<void> {
    if ($x is C) {
      $_ = $this->fail(); // never happens
    }
    return;
  }

  // check that we've correctly imported OutOfBoundsException extends Exception
  public async function hhiDeclsImportedBad(): Awaitable<void> {
    $e = new \OutOfBoundsException();
    try {
      throw $e;
    } catch (\Exception $e) {
      $_ = $this->fail();
    }
  }

  public async function hhiDeclsImportedOK(): Awaitable<void> {
    $e = new \OutOfBoundsException();
    try {
      throw $e;
    } catch (\DivisionByZeroException $e) {
      $_ = $this->fail();
    }
  }

  // two booleans should be equal here
  public async function earlyInstanceOfOK(): Awaitable<void> {
    $x = new F();
    $b1 = $x is D;
    $b2 = $x is E;
    if ($b1 == $b2) { // always succeeds
      return;
    } else {
      $_ = $this->fail();
    }
  }

  // same but with below
  // this is only an FP when we use v>0 in propagate_atom, rather than v != 0
  // TODO: investigate
  public async function earlyInstanceOf2OK_FP(mixed $x): Awaitable<void> {
    if ($x is F) {
      $b1 = $x is D;
      $b2 = $x is E;
      if ($b1 == $b2) { // always succeeds
        return;
      } else {
        $_ = $this->fail();
      }
    } else {
      return;
    }
  }

  // this is an FP because either x is null, in which case both booleans are false
  // or x is not null, in which case they're both true
  // but we don't do case splitting in the solver :-(
  // Note that we *could* do it in the model of boolean equality at the cost
  // of some blowup
  public async function earlyInstanceOf3_FP(mixed $x): Awaitable<void> {
    if ($x is ?F) {
      $b1 = $x is D;
      $b2 = $x is E;
      if ($b1 == $b2) { // always succeeds
        return;
      } else {
        $_ = $this->fail();
      }
    } else {
      return;
    }
  }

  public function return_arraykey(): arraykey {
    return 3;
  }

  public async function hhiAliasImportedOK(mixed $m): Awaitable<void> {
    if ($m is \HH\ClassKind) {
      // here we  should know that's just and alias to string underneath
      if ($m is string) {
        return;
      } else {
        $_ = $this->fail();
      }
    }
    return;
  }
}
