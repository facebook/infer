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

public class NativeMethodTest {

  public static final String TAG = "NativeMethodExample";

  public static final String MAIN_METHOD =
    "codetoanalyze.java.crashcontext.NativeMethodExample.main(java.lang.String[]):void";

  // Currently, native methods are missing their arguments lists.
  public static final String REFLECTION_INVOKE_METHOD =
    "sun.reflect.NativeMethodAccessorImpl.invoke0";

  public static final String FOO_METHOD =
    "codetoanalyze.java.crashcontext.NativeMethodExample.foo():void";

  private static CrashContextResults crashcontext;

  @BeforeClass
  public static void loadResults() throws IOException {
    crashcontext =
      CrashContextResults.loadJSONResults(TAG);
  }

  @Test
  public void shapeOfTheStack() {
      assertThat("The stack trace should contain " + FOO_METHOD,
                 crashcontext.hasStackFrame(FOO_METHOD));
      assertThat("The trace should contain at least one native method ",
                 crashcontext.hasNativeMethodOnStack());
      assertThat("The stack trace should contain " + REFLECTION_INVOKE_METHOD,
                 crashcontext.hasStackFrame(REFLECTION_INVOKE_METHOD));
      assertThat("The stack trace should contain " + MAIN_METHOD,
                 crashcontext.hasStackFrame(MAIN_METHOD));
  }

}
