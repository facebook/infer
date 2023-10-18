/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

/* testing advanced source/sink matchers */
import codetoanalyze.java.pulse.sinks.InferTaintSinks;

public class TaintMatchers {
  void taintedBasedOnClassNameRegexBad() {
    Object src = InferTaint.inferSecretSource();
    InferTaintSinks.sink1(src);
    InferTaintSinks.sink2(src);
  }

  void notTaintedBasedOnClassNameRegexOk() {
    Object src = InferTaint.inferSecretSource();
    InferTaint.sink1(src);
    InferTaint.sink2(src);
  }

  void taintedFromInferBaseSourceBad() {
    InferChildSource ics = new InferChildSource();
    Object source = ics.inferBaseSecretSource();
    InferTaint.inferSensitiveSink(source);
  }

  void notTaintedFromInferBaseNotSourceGood() {
    InferChildSource ics = new InferChildSource();
    Object notSource = ics.inferBaseNotSource();
    InferTaint.inferSensitiveSink(notSource);
  }

  void taintedFromInferChildSourceBad() {
    InferChildSource ics = new InferChildSource();
    Object source = ics.inferChildSecretSource();
    InferTaint.inferSensitiveSink(source);
  }

  void notTaintedFromInferChildNotSourceGood() {
    InferChildSource ics = new InferChildSource();
    Object notSource = ics.inferChildNotSource();
    InferTaint.inferSensitiveSink(notSource);
  }

  void taintedBasedOnClassNameAndMethodRegexBad() {
    Object src = InferTaint.inferSecretSource();
    InferTaint.regexSink(src);
  }

  void notTaintedBasedOnClassNameAndMethodRegexBad() {
    Object src = InferTaint.inferSecretSource();
    InferTaint.notRegexSink(src);
  }

  void taintedBasedOnClassAnnotationBad1() {
    Object src = InferTaintSources.Sources.source1();
    InferTaint.inferSensitiveSink(src);
  }

  void taintedBasedOnClassAnnotationBad2() {
    Object src = InferTaintSources.Sources.source2();
    InferTaint.inferSensitiveSink(src);
  }

  void notTaintedBasedOnClassAnnotationGood() {
    Object src = InferTaintSources.NotSources.notSource();
    InferTaint.inferSensitiveSink(src);
  }

  void taintedBasedOnClassAndProcedureRegexBad1() {
    Object src = InferTaintSources.RegexSources.source1();
    InferTaint.inferSensitiveSink(src);
  }

  void taintedBasedOnClassAndProcedureRegexBad2() {
    Object src = InferTaintSources.RegexSources.source2();
    InferTaint.inferSensitiveSink(src);
  }

  void notTaintedBasedOnClassAndProcedureRegexOk1() {
    Object src = InferTaintSources.RegexSources.notSource();
    InferTaint.inferSensitiveSink(src);
  }

  void notTaintedBasedOnClassAndProcedureRegexOk2() {
    Object src = InferTaintSources.NotSources.sourceButNotReally();
    InferTaint.inferSensitiveSink(src);
  }

  void taintedBasedOnClassAnnotationAndClassAndProcedureRegexBad1() {
    Object src = InferTaintSources.RegexAndAnnotationSources.source1();
    InferTaint.inferSensitiveSink(src);
  }

  void taintedBasedOnClassAnnotationAndClassAndProcedureRegexBad2() {
    Object src = InferTaintSources.RegexAndAnnotationSources.source2();
    InferTaint.inferSensitiveSink(src);
  }

  void notTaintedBasedOnClassAnnotationAndClassAndProcedureRegexGood1() {
    Object src = InferTaintSources.RegexAndAnnotationSources.notSource();
    InferTaint.inferSensitiveSink(src);
  }

  void notTaintedBasedOnClassAnnotationAndClassAndProcedureRegexGood2() {
    Object src = InferTaintSources.RegexAndAnnotationNotSources.notSource();
    InferTaint.inferSensitiveSink(src);
  }

  void notTaintedBasedOnClassAnnotationAndClassAndProcedureRegexGood3() {
    Object src = InferTaintSources.RegexAndAnnotationNotSources.sourceButNotReally();
    InferTaint.inferSensitiveSink(src);
  }
}
