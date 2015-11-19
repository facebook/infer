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

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(TaintTest.class, TaintFile);
  }

  @Test
  public void whenInferRunsOnTaintFileErrorFound()
      throws InterruptedException, IOException, InferException {
    String[] methods = {
        "taintGetHostEquals",
        "taintGetHostCompareTo",
        "taintGetHostEndsWith",
        "taintGetHostStartsWith",
        "taintGetAuthoriyEquals",
        "taintGetAuthorityCompareTo",
        "taintGetAuthorityEndsWith",
        "taintGetAuthorityStartsWith",
        "taintGetProtocolEquals",
        "taintGetProtocolCompareTo",
        "taintGetProtocolEndsWith",
        "taintGetProtocolStartsWith",
        "taintToExternalFormEquals",
        "taintToExternalFormCompareTo",
        "taintToExternalFormEndsWith",
        "taintToExternalFormStartsWith",
        "taintToStringEquals",
        "taintToStringCompareTo",
        "taintToStringEndsWith",
        "taintToStringStartsWith",
        "socketNotVerifiedSimple",
        "socketVerifiedForgotToCheckRetval",
        "socketIgnoreExceptionNoVerify"
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
