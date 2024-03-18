/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

public class TransitiveAccess {
  public interface Callback {
    public void call();
  }

  public static void caller(Callback lambda) {
    lambda.call();
  }

  public static class Sinks {
    public static void safe() {}

    public static void sink() {}
  }

  public static class Base {}

  public static class Context extends Base {
    public static void sourceOk() {
      Sinks.safe();
    }

    public static void sourceBad() {
      Sinks.sink();
    }

    public static void sourceWithLambdaOk() {
      Callback lambda = () -> Sinks.safe();
      lambda.call();
    }

    public static void sourceWithLambdaBad() {
      Callback lambda = () -> Sinks.sink();
      lambda.call();
    }

    // This is currently reported because the generated code
    // corresponding to the lambda falls into the context
    public static void sourceWithLambdaNoCallBad() {
      Callback lambda = () -> Sinks.sink();
    }

    public static void sourceWithLambdaIndirectOk() {
      caller(() -> Sinks.safe());
    }

    public static void sourceWithLambdaIndirectBad() {
      caller(() -> Sinks.sink());
    }
  }
}
