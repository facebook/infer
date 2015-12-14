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
import static utils.matchers.ResultContainsErrorInMethod.contains;
import static utils.matchers.ResultContainsNumberOfErrorsInMethod.containsNumberOfErrors;
import static utils.matchers.ResultContainsZeroErrorsInMethod.containsZeroErrors;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.InferException;
import utils.InferResults;

public class AnalysisStopsTest {

  public static final String AnalysisStops =
      "infer/tests/codetoanalyze/java/infer/AnalysisStops.java";

  public static final String DIVIDE_BY_ZERO = "DIVIDE_BY_ZERO";
  public static final String NULL_DEREFERENCE = "NULL_DEREFERENCE";

  private static InferResults inferResults;

  @BeforeClass
  public static void loadResults() throws InterruptedException, IOException {
    inferResults = InferResults.loadInferResults(AnalysisStopsTest.class, AnalysisStops);
  }

  @Test
  public void skipPtrDerefDoesntCauseLocalFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "skipPointerDerefMayCauseLocalFalseNegative"
        )
    );
  }

  @Test
  public void skipPtrDerefDoesntCauseCalleeFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "skipPointerDerefMayCauseCalleeFalseNegative"
        )
    );
  }

  @Test
  public void skipPtrDerefDoesntCauseCalleeFalsePositive()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain no errors",
        inferResults,
        containsZeroErrors(
            AnalysisStops,
            "skipPointerDerefMayCauseCalleeFalsePositive"));
  }

  @Test
  public void skipPtrDerefDoesntCauseInterprocFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "skipPointerDerefMayCauseInterprocFalseNegative"
        )
    );
  }

  @Test
  public void castUndefinedObjDoesntCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "castFailureOnUndefinedObjMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void callOnCastUndefinedObjDoesntCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "callOnCastUndefinedObjMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void callOnUndefinedObjDoesntCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "callOnUndefinedObjMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void callOnUndefinedObjDoesntCauseFalsePositive()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain no errors",
        inferResults,
        containsZeroErrors(
            AnalysisStops,
            "callOnUndefinedObjMayCauseFalsePositive"));
  }

  @Test
  public void fieldWriteOnUndefinedObjDoesntCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "fieldWriteOnUndefinedObjMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void fieldWriteOnUndefinedObjDoesntCauseFalsePositive()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain no errors",
        inferResults,
        containsZeroErrors(
            AnalysisStops,
            "fieldWriteOnUndefinedObjMayCauseFalsePositive"));
  }

  @Test
  public void fieldReadOnUndefinedObjDoesntCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "fieldReadOnUndefinedObjMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void fieldReadOnUndefinedObjDoesntCauseFalsePositive()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain no errors",
        inferResults,
        containsZeroErrors(
            AnalysisStops,
            "fieldReadOnUndefinedObjMayCauseFalsePositive"));
  }

  @Test
  public void recursiveAngelicTypesDontCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "recursiveAngelicTypesMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void recursiveAngelicTypesDontCauseFalsePositive()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain no errors",
        inferResults,
        containsZeroErrors(
            AnalysisStops,
            "recursiveAngelicTypeMayCauseFalsePositive"));
  }

  @Test
  public void infiniteMaterializationDoesntCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "infiniteMaterializationMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void infiniteMaterializationDoesntCauseFalsePositive()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain no errors",
        inferResults,
        containsZeroErrors(
            AnalysisStops,
            "infiniteMaterializationMayCauseFalsePositive"));
  }

  @Test
  public void primitiveFieldOfAngelicObjDoesntCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        containsNumberOfErrors(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "primitiveFieldOfAngelicObjMayCauseFalseNegative",
            2
        )
    );
  }

  @Test
  public void primitiveFieldOfAngelicObjDoesntCauseFalsePositive()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain no errors",
        inferResults,
        containsZeroErrors(
            AnalysisStops,
            "primitiveFieldOfAngelicObjMayCauseFalsePositive"));
  }

  @Test
  public void heapFieldOfAngelicObjDoesntCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "heapFieldOfAngelicObjMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void heapFieldOfAngelicObjDoesntCauseFalsePositive()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain no errors",
        inferResults,
        containsZeroErrors(
            AnalysisStops,
            "heapFieldOfAngelicObjMayCauseFalsePositive"));
  }

  @Test
  public void fieldReadAfterCastDoesntCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "fieldReadAferCastMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void fieldReadInCalleeDoesntCauseFalsePositive()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain no errors",
        inferResults,
        containsZeroErrors(
            AnalysisStops,
            "fieldReadInCalleeMayCauseFalsePositive"));
  }

  // these do not work yet
  @Test
  public void fieldReadInCalleeDoesntCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            NULL_DEREFERENCE,
            AnalysisStops,
            "fieldReadInCalleeMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void fieldReadInCalleeWithAngelicObjFieldDoesntCauseFalsePositive()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain no errors",
        inferResults,
        containsZeroErrors(
            AnalysisStops,
            "fieldReadInCalleeWithAngelicObjFieldMayCauseFalsePositive"));
  }

  @Test
  public void fieldReadInCalleeWithAngelicObjFieldDoesntCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            NULL_DEREFERENCE,
            AnalysisStops,
            "fieldReadInCalleeWithAngelicObjFieldMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void accessPathInCalleeDoesntCauseFalsePositive()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain no errors",
        inferResults,
        containsZeroErrors(
            AnalysisStops,
            "accessPathInCalleeMayCauseFalseNegative"));
  }

  @Test
  public void accessPathInCalleeDoesntCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            NULL_DEREFERENCE,
            AnalysisStops,
            "accessPathInCalleeMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void skipFunctionInLoopShouldNotCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            NULL_DEREFERENCE,
            AnalysisStops,
            "skipFunctionInLoopMayCauseFalseNegative"
        )
    );
  }

  @Test
  public void specInferenceFailureShouldNotCauseFalseNegative()
      throws InterruptedException, IOException, InferException {
    assertThat(
        "Results should contain error",
        inferResults,
        contains(
            DIVIDE_BY_ZERO,
            AnalysisStops,
            "specInferenceMayFailAndCauseFalseNegative"
        )
    );
  }

}
