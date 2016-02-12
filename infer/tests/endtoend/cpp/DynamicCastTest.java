/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.cpp;

import static org.hamcrest.MatcherAssert.assertThat;
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsExactly.containsExactly;
import static utils.matchers.ResultContainsNoErrorInMethod.doesNotContain;

import com.google.common.collect.ImmutableList;

import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.InferResults;
import utils.InferRunner;

public class DynamicCastTest {

  public static final String FILE =
      "infer/tests/codetoanalyze/cpp/errors/subtyping/dynamic_cast.cpp";

  private static ImmutableList<String> inferCmd;

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";

  public static final String CLASS_CAST_EXCEPTION = "CLASS_CAST_EXCEPTION";

  @ClassRule
  public static DebuggableTemporaryFolder folder =
      new DebuggableTemporaryFolder();

  @BeforeClass
  public static void runInfer() throws InterruptedException, IOException {
    inferCmd = InferRunner.createCPPInferCommand(folder, FILE);
  }

  @Test
  public void whenInferRunsOnWrongCastOfArgumentPointerThenDivideByZeroIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Results should contain divide by zero, it shows a wrong cast to pointer to class",
        inferResults,
        contains(DIVIDE_BY_ZERO, FILE, "wrongCastOfArgumentPointer"));
  }

  @Test
  public void whenInferRunsOnWrongPointerCastThenDivideByZeroIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Results should contain divide by zero, it shows a wrong cast to pointer to class",
        inferResults,
        contains(DIVIDE_BY_ZERO, FILE, "wrongPointerCast"));
  }

  @Test
  public void whenInferRunsOnRightPointerCastThenDivideByZeroIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Results should contain divide by zero, it shows a correct cast to pointer to class",
        inferResults,
        contains(DIVIDE_BY_ZERO, FILE, "rightPointerCast"));
  }

  @Test
  public void whenInferRunsOnRightReferenceCastThenClassCastExceptionIsNotFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Results should not contain class cast exception",
        inferResults,
        doesNotContain(CLASS_CAST_EXCEPTION, FILE, "rightReferenceCast"));
  }

  @Test
  public void whenInferRunsOnWrongReferenceCastThenClassCastExceptionIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Results should contain class cast exception",
        inferResults,
        contains(CLASS_CAST_EXCEPTION, FILE, "wrongReferenceCast"));
  }

  @Test
  public void whenInferRunsOnWrongReferenceCastNotAssignedThenClassCastExceptionIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Results should contain class cast exception",
        inferResults,
        contains(CLASS_CAST_EXCEPTION, FILE, "wrongReferenceCastNotAssigned"));
  }

  @Test
  public void whenInferRunsOnWrongCastOfArgumentReferenceThenClassCastExceptionIsFound()
      throws InterruptedException, IOException, InferException {
    InferResults inferResults = InferRunner.runInferCPP(inferCmd);
    assertThat(
        "Results should contain class cast exception",
        inferResults,
        contains(CLASS_CAST_EXCEPTION, FILE, "wrongCastOfArgumentReference"));
  }

}
