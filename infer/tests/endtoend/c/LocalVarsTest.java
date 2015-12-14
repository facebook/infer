/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.c;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsOnlyTheseErrors.containsOnly;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class LocalVarsTest {

  public static final String local_vars_file =
      "local_vars/local_vars.c";

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";

  private static InferResults inferResults;

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferResults = InferResults.loadCInferResults(LocalVarsTest.class, local_vars_file);
  }

  @Test
  public void whenInferRunsOnLocal_vars_mThenDivideByZeroIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain divide by zero error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            local_vars_file,
            "m"
        )
    );
  }

  @Test
  public void whenInferRunsOnLocal_vars_mmThenDivideByZeroIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain divide by zero error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            local_vars_file,
            "mm"
        )
    );
  }

  @Test
  public void whenInferRunsOnLocal_vars_tThenDivideByZeroIsFound()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain divide by zero error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            local_vars_file,
            "t"
        )
    );
  }


  @Test
  public void whenInferRunsOnNullPointerDereferenceThenOnlyTheExpectedErrorsAreFound()
      throws InterruptedException, IOException, InferException {
    String[] expectedProcedures = {"m", "mm", "t"};
    assertThat(
        "No unexpected errors should be found", inferResults,
        containsOnly(
            DIVIDE_BY_ZERO,
            local_vars_file,
            expectedProcedures));
  }

}
