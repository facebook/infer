/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package java.source.parser;

public class Main {

  // Tests for annotations
  static interface Interface {
    public Object foo();
  }

  @ExampleCustomAnnotation(name = "Chaitanya", address = "Agra, India")
  class AnnotatedClass {
    @Deprecated
    void method1() {
      return;
    }

    @SuppressWarnings("deprecation")
    void method2() {
      return;
    }
  }

  static class Impl implements Interface {
    @Override
    public Object foo() {
      return null;
    }
  }

  // tests for anonymous inner classes
  static class MyThread {
    MyThread(Object o) {}

    public static void main(String[] args) {

      Thread t1 =
          new Thread() {
            public void run() {
              System.out.println("Child Thread");
            }
          };
      t1.start();

      Thread t2 =
          new Thread(
              new Runnable() {
                public void run() {
                  System.out.println("Child Thread");
                }
              });
      t2.start();

      // nested anonymous classes
      MyThread mt =
          new MyThread(
              new Object() {
                private int counter;

                int get_counter() {
                  return this.counter;
                }
              }) {
            private String label;

            String get_label() {
              return this.label;
            }
          };
    }
  }

  // tests for enum

  public enum Block {
    NONE(""),

    WALL("Wall") {
      @Override
      public boolean good() {
        return true;
      }
    },

    PIT("Pit") {
      @Override
      public boolean good() {
        return true;
      }
    },

    FOG("Fog") {
      @Override
      public boolean good() {
        return true;
      }
    };

    private class C {};

    private String name;

    private Block(String name) {
      this.name = name;
    }

    public String getName() {
      return this.name;
    }

    public boolean good() {
      return false;
    }
  }
}
