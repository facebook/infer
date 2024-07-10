/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.annotreach

@Target(AnnotationTarget.CLASS, AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.RUNTIME)
annotation class UserDefinedSource

@Target(AnnotationTarget.CLASS, AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.RUNTIME)
annotation class UserDefinedSink

@Target(AnnotationTarget.CLASS, AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.RUNTIME)
annotation class UserDefinedSanitizer

class CustomAnnotations {
  @UserDefinedSink fun sink(): Unit {}

  fun notSink(): Unit {}

  @UserDefinedSource
  fun sourceBad(): Unit {
    sink()
  }

  @UserDefinedSource
  fun sourceOk(): Unit {
    notSink()
  }

  @UserDefinedSanitizer
  fun canCallSink(): Unit {
    sink()
  }

  @UserDefinedSource
  fun sourceWithSanitizerOk(): Unit {
    canCallSink()
  }

  fun notSourceOk(): Unit {
    sink()
  }

  @UserDefinedSource @UserDefinedSink fun sourceAndSinkAtTheSameTimeBad(): Unit {}
}
