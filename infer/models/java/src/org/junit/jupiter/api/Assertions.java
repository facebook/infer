package org.junit.jupiter.api;

import static com.facebook.infer.builtins.InferBuiltins.assume;
import javax.annotation.Nullable;

public class Assertions {

  public static void assertNotNull(@Nullable Object object) {
    assume(object != null);
  }

  public static void assertNotNull(@Nullable Object object, String message) {
    assume(object != null);
  }

  public static void assertNull(@Nullable Object object) {
    assume(object == null);
  }

  public static void assertNull(@Nullable Object object, String message) {
    assume(object == null);
  }

}
