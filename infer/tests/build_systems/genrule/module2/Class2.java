package genrule.module2;

import genrule.module1.Class1;

public class Class2 {

  void interTargetNPE() {
    Object obj = Class1.returnsNull();
    obj.toString();
  }

  void localNPE2() {
    Object obj = null;
    obj.toString();
  }

}
