/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import com.facebook.infer.annotation.Nullsafe;

class OtherClass {

  OtherClass x = null;

  OtherClass canReturnNull() {
    return this.x;
  }

  String buggyMethodBad() {
    OtherClass o = new OtherClass();
    return o.canReturnNull().toString();
  }
}

/*
 * Pulse will not report NPEs on @Nullsafe classes if flag
 * --pulse-nullsafe-report-npe is set to false
 */

@Nullsafe(Nullsafe.Mode.LOCAL)
class NullsafeExampleLocal {
  void testingNullsafeLocalMode() {
    OtherClass o = new OtherClass();
    o = o.canReturnNull();
    o.getClass();
  }
}

@Nullsafe(Nullsafe.Mode.STRICT)
class NullsafeExampleStrict {
  void testingNullsafeStrictMode() {
    OtherClass o = new OtherClass();
    o = o.canReturnNull();
    o.toString();
  }
}
