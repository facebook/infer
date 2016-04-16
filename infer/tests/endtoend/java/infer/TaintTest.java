/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsExactly.containsExactly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class TaintTest {

  public static final String TaintFile =
      "infer/tests/codetoanalyze/java/infer/TaintExample.java";

  public static final String TAINTED_VALUE = "TAINTED_VALUE_REACHING_SENSITIVE_FUNCTION";

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(TaintTest.class, TaintFile);
  }

  @Test
  public void whenInferRunsOnTaintFileErrorFound()
      throws InterruptedException, IOException, InferException {
    String[] methods = {
        "socketNotVerifiedSimple",
        "socketVerifiedForgotToCheckRetval",
        "socketIgnoreExceptionNoVerify",
        "callReadInputStreamCauseTaintError",
        "taintingShouldNotPreventInference1",
        "taintingShouldNotPreventInference2",
        "simpleTaintErrorWithModelMethods",
        "interprocTaintErrorWithModelMethods1",
        "interprocTaintErrorWithModelMethods2",
        "interprocTaintErrorWithModelMethods3",
        "simpleTaintErrorWithModelMethodsUndefined",
        "interprocTaintErrorWithModelMethodsUndefined1",
        "interprocTaintErrorWithModelMethodsUndefined2",
        "interprocTaintErrorWithModelMethodsUndefined3",
        "contentValuesPutWithTaintedString",
        "testPrivacySourceAnnot",
        "testPrivacySinkAnnot1",
        "testPrivacySinkAnnot3"
    };

    assertThat(
        "Results should contain tainted value reaching sensitive function.",
        inferResults,
        containsExactly(
            TAINTED_VALUE,
            TaintFile,
            methods
        )
    );
  }

}
