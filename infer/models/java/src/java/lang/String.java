/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.lang;

import com.facebook.infer.builtins.InferUndefined;

public final class String {

  private final char[] value;
  private final int offset;
  private final int count;

  public int length() {
    if (this == "") return 0;
    else {
      return InferUndefined.nonneg_int();
    }
  }

  public String() {
    this.offset = 0;
    this.count = 0;
    this.value = new char[0];
  }

  public String(byte bytes[]) {
    this(bytes, 0, bytes.length);
  }

  public String(byte bytes[], int offset, int length) {
    checkBounds(bytes, offset, length);
    char[] v = new char[bytes[0]];
    /** yes, this could be improved * */
    this.offset = 0;
    this.count = v.length;
    this.value = v;
  }

  private static void checkBounds(byte[] bytes, int offset, int length) {
    if (length < 0) throw new StringIndexOutOfBoundsException(length);
    if (offset < 0) throw new StringIndexOutOfBoundsException(offset);
    if (offset > bytes.length - length) throw new StringIndexOutOfBoundsException(offset + length);
  }

  public boolean equals(Object anObject) {
    if (this == anObject) {
      return true;
    } else {
      return InferUndefined.boolean_undefined();
    }
  }
}
