/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
// We want to find flows from source() to sink() that satisfy two conditions:
// (1) some intermediate value has type String; and
// (2) no intermediate value has type Boolean or Enum (including subtypes).
abstract class FlowTypes {

  // TODO: Topl considers the case in which the result of eofA is the same (or points to) the
  // result of source. It doesn't realize that this can't happen. But it also can't prove that it
  // happens. So it decides to wait, in the hope it will prove it at the callsite, resulting in a
  // latent issue being reported here. Of course, it won't be able to show it. Can we get rid of
  // such latent issues that can't gain anything from more info at the callsite?
  void fpl_faOk() {
    sink(stringOfE(eOfA(source())));
  }

  // TODO: Same problem as above.
  void fpl_fbOk() {
    sink(eOfString(stringOfA(source())));
  }

  void fcBad() {
    sink(stringOfA(source()));
  }

  void fdBad() {
    sink(stringOfB(bOfA(source())));
  }

  void feBad() {
    sink(aOfString(stringOfB(bOfA(source()))));
  }

  void ffBad() {
    A x = source();
    E y = eOfA(x);
    String z = stringOfE(y);
    String u = stringOfA(x);
    sink(z); // OK
    sink(u); // NOK
  }

  void fgOk() {
    sink(sanitizer(source()));
  }

  // like faOk, but with Boolean as intermediary instead of E
  void fpl_fhOk() {
    sink(stringOfBoolean(booleanOfA(source())));
  }

  // like faOk, but with primitive boolean as intermediary instead of E
  void fpl_fiOk() {
    sink(stringOfBool(boolOfA(source())));
  }

  abstract void sink(Object x);

  abstract A source();

  abstract A aOfString(String x);

  abstract B bOfA(A x);

  abstract Boolean booleanOfA(A x);

  abstract E eOfA(A x);

  abstract E eOfString(String x);

  abstract String sanitizer(A x);

  abstract String stringOfA(A x);

  abstract String stringOfB(B x);

  abstract String stringOfBool(boolean x);

  abstract String stringOfBoolean(Boolean x);

  abstract String stringOfE(E x);

  abstract boolean boolOfA(A x);
}

class A {}

class B {}

enum E {}
