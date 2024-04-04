/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

public class TransitiveAccess {
  public interface Callback {
    public void call();
  }

  public static void caller(Callback lambda) {
    lambda.call();
  }

  @Target({ElementType.METHOD})
  @Retention(RetentionPolicy.CLASS)
  @interface SinkAnno {}

  @Target({ElementType.METHOD})
  @Retention(RetentionPolicy.CLASS)
  @interface SourceAnno {}

  @Target({ElementType.METHOD})
  @Retention(RetentionPolicy.CLASS)
  @interface SomeRandomAnno {}

  public static class Sinks {
    public static void safe() {}

    public static void sink() {}

    @SinkAnno
    public static void funcWithAnno() {}

    @SomeRandomAnno
    public static void safeWithAnno() {}

    public static void sink_if_arg_true(boolean arg) {
      if (arg) sink();
      else safe();
    }
  }

  public static class Base {}

  public static class Context extends Base {
    public static void sourceOk() {
      Sinks.safe();
    }

    public static void sourceBad() {
      Sinks.sink();
    }

    public static void sourceConditionalOk() {
      Sinks.sink_if_arg_true(false);
    }

    public static void sourceConditionalBad() {
      Sinks.sink_if_arg_true(true);
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

    public static void sourceCallsFuncWithAnnoBad() {
      Sinks.funcWithAnno();
    }

    public static void sourceCallsFuncWithAnnoOk() {
      Sinks.safeWithAnno();
    }
  }

  @SourceAnno
  public static void sourceWithAnnoBad() {
    Sinks.sink();
  }

  @SourceAnno
  public static void sourceWithAnnoOk() {
    Sinks.safe();
  }

  @SourceAnno
  public static void sourceWithAnnoAndLambdaBad() {
    caller(() -> Sinks.sink());
  }

  @SourceAnno
  public static void sourceWithAnnoAndLambdaOk() {
    caller(() -> Sinks.safe());
  }

  @SourceAnno
  public static void sourceWithAnnoAndLambdaNoCallOk() {
    Callback lambda = () -> Sinks.sink();
  }

  @SourceAnno
  public static void sourceWithChainOfCallsBad() {
    f1();
  }

  public static void f1() {
    f2();
  }

  public static void f2() {
    f3();
  }

  public static void f3() {
    Sinks.funcWithAnno();
  }
}
