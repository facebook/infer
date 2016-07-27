/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package endtoend.java.crashcontext;

import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;

import utils.CrashContextResults;

public class BranchingCallsTest {

  public static final String TAG = "BranchingCallsExample";

  public static final String MAIN_METHOD =
    "codetoanalyze.java.crashcontext.BranchingCallsExample.main(java.lang.String[]):void";

  public static final String FOO_METHOD =
    "codetoanalyze.java.crashcontext.BranchingCallsExample.foo():void";

  public static final String PRE_BAR_METHOD =
    "codetoanalyze.java.crashcontext.BranchingCallsExample.pre_bar():void";

  public static final String BAR_METHOD =
    "codetoanalyze.java.crashcontext.BranchingCallsExample.bar():void";

  public static final String POST_BAR_METHOD =
    "codetoanalyze.java.crashcontext.BranchingCallsExample.post_bar():void";

  public static final String TO_STRING_METHOD =
    "java.lang.String.toString():java.lang.String";

  private static CrashContextResults crashcontext;

  @BeforeClass
  public static void loadResults() throws IOException {
    crashcontext =
      CrashContextResults.loadJSONResults(TAG);
  }

  @Test
  public void shapeOfTheStack() {
      assertThat("The stack trace should contain " + BAR_METHOD,
                 crashcontext.hasStackFrame(BAR_METHOD, 0));
      assertThat("The stack trace should contain " + FOO_METHOD,
                 crashcontext.hasStackFrame(FOO_METHOD, 1));
      assertThat("The stack trace should contain " + MAIN_METHOD,
                 crashcontext.hasStackFrame(MAIN_METHOD, 2));
  }

  @Test
  public void toStringMethodIsFound() {
    assertThat("Method " + TO_STRING_METHOD + " should be part of the context",
               crashcontext.hasMethod(TO_STRING_METHOD));
    assertThat("Method " + TO_STRING_METHOD + " should be reachable in the " +
               "context tree from " + BAR_METHOD,
               crashcontext.hasPath(BAR_METHOD, TO_STRING_METHOD));
  }

  @Test
  public void preBarMethodIsFound() {
    assertThat("Method " + PRE_BAR_METHOD + " should be part of the context",
               crashcontext.hasMethod(PRE_BAR_METHOD));
    assertThat("Method " + PRE_BAR_METHOD + " should be reachable in the " +
               "context tree from " + FOO_METHOD,
               crashcontext.hasPath(FOO_METHOD, PRE_BAR_METHOD));
  }

  @Test
  public void postBarMethodIsOmitted() {
    assertThat("Method " + POST_BAR_METHOD + " shouldn't be part of the context",
               crashcontext.hasNotMethod(POST_BAR_METHOD));
  }

}
