package genrule.module1;

import genrule.annotations.Nullable;

public class Class1 {

  @Nullable
  public static String returnsNull() {
    return null;
  }

  void localNPE1() {
    Object obj = null;
    obj.toString();
  }

}
