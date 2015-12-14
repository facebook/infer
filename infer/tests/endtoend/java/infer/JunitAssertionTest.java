/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.infer;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class JunitAssertionTest {

  public static final String JunitAssertionFile =
      "infer/tests/codetoanalyze/java/infer/JunitAssertion.java";

  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(JunitAssertionTest.class, JunitAssertionFile);
  }

  @Test
  public void inferShouldUseAssertedPredicate()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "Results should contain null pointer exception error",
        inferResults,
        doesNotContain(
            NULL_DEREFERENCE,
            JunitAssertionFile,
            "consistentAssertion"
        )
    );
  }

  @Test
  public void inferShouldUseAssertionInconsistency()
      throws IOException, InterruptedException, InferException {
    assertThat(
        "Results should contain null pointer exception error",
        inferResults,
        contains(
            NULL_DEREFERENCE,
            JunitAssertionFile,
            "inconsistentAssertion"
        )
    );
  }

}
