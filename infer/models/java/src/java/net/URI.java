package java.net;

import static com.facebook.infer.builtins.InferBuiltins.assume;
import javax.annotation.Nullable;

public final class URI {

  public URI(@Nullable String uri) {
    assume(uri != null);
  }

}
