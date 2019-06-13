/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
// makes it easier to mock graphql types
package com.facebook.graphql.model;

import java.util.List;

abstract class A {
  abstract A getA();

  abstract B getB();

  abstract C getC();
}

abstract class B {
  abstract C getC();
}

abstract class C {
  abstract D getD();
}

class D {}

abstract class GraphQLStory {

  public abstract List getActors();
}

class LithoTest {

  void /*basic chain*/ onCreateLayout(A a) {
    a.getB().getC().getD();
  }

  void /*sibling chain*/ onCreateLayout(A a, int i) {
    a.getB().getC().getD();
    a.getC().getD();
  }

  void /*split chain*/ onCreateLayout(A a, int i1, int i2) {
    B b = a.getB();
    C c = b.getC();
    c.getD();
  }

  void chainFromActual1(B b) {
    b.getC().getD();
  }

  void chainFromActual2(C c) {
    c.getD();
  }

  void /*chain rooted in actual*/ onCreateLayout(A a, boolean b) {
    chainFromActual1(a.getB());
  }

  void /*local chain + interproc chain*/ onCreateLayout(A a, char ch) {
    C c = a.getB().getC();
    chainFromActual2(c);
  }

  // conditional getters
  static GraphQLStory getPrimaryActor(GraphQLStory story) {
    List actors = story.getActors();
    return actors != null && actors.size() > 0 ? (GraphQLStory) actors.get(0) : null;
  }

  void /*conditional getters on formal*/ onCreateLayout(GraphQLStory story) {
    getPrimaryActor(story).toString();
  }

  static native GraphQLStory getStory();

  void /*conditional getters on local*/ onCreateLayout() {
    GraphQLStory story = getStory();
    getPrimaryActor(story).toString();
  }

  void /*cycle*/ onCreateLayout(A a, float f) {
    a = a.getA();
  }

  void cycle(A a) {
    a = a.getA();
  }

  void /*interprocedural cycle*/ onCreateLayout(A a, double d) {
    cycle(a);
  }
}
