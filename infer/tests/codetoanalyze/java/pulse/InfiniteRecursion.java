/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

public class InfiniteRecursion {

  public void basicRecBad(int i) {
    basicRecBad(i);
  }

  static class PingPongBad {
    public static void ping(int i) {
      pong(i);
    }

    public static void pong(int i) {
      ping(i);
    }
  }

  static class VirtualCalls {
    static class A {
      public void run() {
        process();
      }

      public void process() {}
    }

    static final class Ok extends A {
      public void run() {}

      public void process() {
        run();
      }
    }

    static final class Bad extends A {
      public void process() {
        run();
      }
    }
  }
}
