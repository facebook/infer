package org.testng;

import static com.facebook.infer.builtins.InferBuiltins.assume;
import javax.annotation.Nullable;

public class AssertJUnit {

  public static void assertNotNull(@Nullable Object object) {
    assume(object != null);
  }

  public static void assertNotNull(String message, @Nullable Object object) {
    assume(object != null);
  }

  public static void assertNull(@Nullable Object object) {
    assume(object == null);
  }

  public static void assertNull(String message, @Nullable Object object) {
    assume(object == null);
  }

}
