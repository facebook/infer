/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.topl;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.CLASS)
@interface Danger {}

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.CLASS)
@interface Safe {}

public class Annot {
  void testA_Bad(A x) {
    sink(x.sourceUnlessSafe());
  }

  void testB_Ok(B x) {
    sink(x.sourceUnlessSafe());
  }

  void testC_Ok(C x) {
    sink(x.sourceUnlessSafe());
  }

  void testD_Ok(B x) {
    sink(x.sourceUnlessSafe());
  }

  void testE_Ok(E x) {
    sink(x.sourceIfDanger());
  }

  void testF_Bad(F x) {
    sink(x.sourceIfDanger());
  }

  void testG_Bad(G x) {
    sink(x.sourceIfDanger());
  }

  void testH_Bad(H x) {
    sink(x.sourceIfDanger());
  }

  static void sink(Object _x) {}
}

class A {
  Object sourceUnlessSafe() {
    return new Object();
  }
}

class B {
  @Safe
  Object sourceUnlessSafe() {
    return new Object();
  }
}

@Safe
class C {
  Object sourceUnlessSafe() {
    return new Object();
  }
}

@Safe
class Dbase {}

class D extends Dbase {
  Object sourceUnlessSafe() {
    return new Object();
  }
}

class E {
  Object sourceIfDanger() {
    return new Object();
  }
}

class F {
  @Danger
  Object sourceIfDanger() {
    return new Object();
  }
}

@Danger
class G {
  Object sourceIfDanger() {
    return new Object();
  }
}

@Danger
class Hbase {}

class H extends Hbase {
  Object sourceIfDanger() {
    return new Object();
  }
}
