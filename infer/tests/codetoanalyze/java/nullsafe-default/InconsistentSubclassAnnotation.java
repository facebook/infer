/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import external.library.SomeExternalClass;
import javax.annotation.Nullable;

interface VariousMethods {
  String valBoth(String arg);

  @Nullable
  String nullableReturn(String arg);

  String nullableArg(@Nullable String arg);

  @Nullable
  String nullableBoth(@Nullable String arg);
}

interface Overloads {
  String overload(int arg);

  String overload(@Nullable String arg);

  String overload(String arg1, int arg2);

  String overload(String arg1, String arg2);
}

// Check return annotations

abstract class ReturnValToNullBAD implements VariousMethods {
  @Nullable
  public String valBoth(String arg) {
    return null;
  }
}

abstract class ReturnNullToValOK implements VariousMethods {
  public abstract String nullableReturn(String arg);
}

abstract class ReturnValFromValAndNullFromNullOK implements VariousMethods {
  @Nullable
  public String nullableReturn(String arg) {
    return null;
  }

  public String valBoth(String arg) {
    return arg;
  }
}

abstract class AbstractReturnValToNullFN implements VariousMethods {
  // An abstract override method with inconsistent signature is not reported
  @Nullable
  public abstract String valBoth(String arg);
}

// Check parameter annotations

abstract class ArgValToNullOK implements VariousMethods {
  public String valBoth(@Nullable String arg) {
    return "OK";
  }
}

abstract class ArgNullToValBAD implements VariousMethods {
  public String nullableArg(String arg) {
    return arg;
  }
}

abstract class ArgNullToValForInterfaceInAnotherFileBAD
    implements InconsistentSubclassAnnotationInterface {
  public String implementInAnotherFile(String s) {
    return "BAD";
  }
}

abstract class ArgValToValAndNullToNullOK implements VariousMethods {
  public String valBoth(String arg) {
    return arg;
  }

  @Nullable
  public String nullableBoth(@Nullable String arg) {
    return arg;
  }
}

// Check overrides + overloads

// This is 'good' cases (should be OK except 1 FP due to broken is_override logic)
abstract class OverrideExistingCorrectlyOK implements Overloads {
  // This is FP
  public String overload(int arg) {
    return "OK";
  }

  public String overload(@Nullable String arg) {
    return arg;
  }

  public String overload(String arg1, int arg2) {
    return arg1;
  }

  public String overload(String arg1, String arg2) {
    return arg1;
  }
}

// These are FP cases that get reported due to broken is_override logic
abstract class NoOverrideSinceDifferentTypesFP implements Overloads {
  @Nullable
  public String overload(Object arg) {
    return arg.toString();
  }

  public String overload(Double arg) {
    return arg.toString();
  }
}

// This is just a smoke test to check that incorrect overrides of overloaded methods get reported
abstract class OverloadExistingIncorrectBAD implements Overloads {
  @Nullable
  public String overload(String arg1, String arg2) {
    return null;
  }
}

// Check constructors

class ConstructorsAreExcluded {
  class Base {
    Base(@Nullable String s) {}
  }

  class Derived extends Base {
    Derived(String s) { // OK: there's no sub-typing between constructors
      super(s);
    }
  }
}

// Check interop with external libraries

class ExtendsExternalLibrary extends SomeExternalClass {

  @Override
  public @Nullable Object externalMethod1() {
    // subtyping error on the return type not reported as we cannot
    // rely on the external libraries to be correctly annotated
    return null;
  }

  @Override
  public void externalMethod2(Object object) {
    // subtyping error on the parameter type are reported
  }
}
