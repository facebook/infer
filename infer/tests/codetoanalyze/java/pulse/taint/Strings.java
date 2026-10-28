/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.util.Formatter;

/**
 * a lot of tainted values are strings, so propagation through StringBuilder's and the like is very
 * important.
 */
public class Strings {

  void viaStringBuilderSugarBad() {
    Object source = InferTaint.inferSecretSource();
    InferTaint.inferSensitiveSink(source + "");
  }

  void viaStringBuilderBad() {
    Object source = InferTaint.inferSecretSource();
    StringBuilder builder = new StringBuilder();
    InferTaint.inferSensitiveSink(builder.append(source).append("").toString());
  }

  void viaStringBuilderIgnoreReturnBad() {
    Object source = InferTaint.inferSecretSource();
    StringBuilder builder = new StringBuilder();
    // builder should be tainted after this call even though we ignore the return value
    builder.append(source);
    InferTaint.inferSensitiveSink(builder.toString());
  }

  void viaStringBufferBad() {
    Object source = InferTaint.inferSecretSource();
    StringBuffer buffer = new StringBuffer();
    InferTaint.inferSensitiveSink(buffer.append("").append(source).toString());
  }

  void viaStringBufferIgnoreReturnBad() {
    Object source = InferTaint.inferSecretSource();
    StringBuffer buffer = new StringBuffer();
    buffer.append(source);
    InferTaint.inferSensitiveSink(buffer.toString());
  }

  void viaFormatterBad() {
    Object source = InferTaint.inferSecretSource();
    Formatter formatter = new Formatter();
    InferTaint.inferSensitiveSink(formatter.format("%s", source).toString());
  }

  void viaFormatterIgnoreReturnBad() {
    Object source = InferTaint.inferSecretSource();
    Formatter formatter = new Formatter();
    formatter.format("%s", source);
    InferTaint.inferSensitiveSink(formatter.toString());
  }

  void viaStringFormatVarArgsDirectBad() {
    Object source = InferTaint.inferSecretSource();
    String tainted = String.format("%s%s", "hi", source);
    InferTaint.inferSensitiveSink(tainted);
  }

  void viaStringFormatVarArgsIndirect(Object param) {
    String tainted = String.format("%s%s", "hi", param);
    InferTaint.inferSensitiveSink(tainted);
  }

  void viaStringFormatVarArgsIndirectBad() {
    viaStringFormatVarArgsIndirect(InferTaint.inferSecretSource());
  }

  void compareConstantWithTaintedOk() {
    String source = (String) InferTaint.inferSecretSource();
    String constant = "name";
    if (constant.equals(source) || constant.startsWith(source) || constant.indexOf(source) > 0) {
      InferTaint.inferSensitiveSink(constant);
    }
    InferTaint.inferSensitiveSink(constant);
  }

  void compareTaintedWithConstantBad() {
    String source = (String) InferTaint.inferSecretSource();
    if (source.equals("name")) {
      return;
    }
    InferTaint.inferSensitiveSink(source);
  }

  // primitive values are not tracked as tainted: what matters is how they are checked where they
  // are used
  void charOfTaintedOk() {
    String source = (String) InferTaint.inferSecretSource();
    char c = source.charAt(0);
    InferTaint.inferSensitiveSink(String.valueOf(c));
  }

  void indexInTaintedOk() {
    String source = (String) InferTaint.inferSecretSource();
    int i = source.indexOf(",");
    InferTaint.inferSensitiveSink(String.valueOf(i));
  }
}
