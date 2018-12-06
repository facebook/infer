/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import org.json.JSONArray;

class UnknownCallsTest {

  private int mBytesToRead;

  public void jsonArray_linear(JSONArray jsonArray) {
    int length = jsonArray.length();
    for (int i = 0; i < length; ++i) {}
  }

  // call to JSONArray.length is not modeled, hence the result will
  // not be invariant. Therefore we get quadratic bound.
  public void jsonArray_quadratic(JSONArray jsonArray) {
    for (int i = 0; i < jsonArray.length(); ++i) {}
  }

  public int read_sum_cost(
      InputStream in, byte[] buffer, int byteOffset, int byteCount, ArrayList<Integer> list)
      throws IOException {
    int maxBytesToRead = Math.min(byteCount, mBytesToRead);
    int bytesRead = in.read(buffer, byteOffset, maxBytesToRead);

    for (int index = 0; index < bytesRead + maxBytesToRead; ++index) {}
    return 0;
  }

  // Expected: Max(Math,min(...), InputStream.read(....)) but we get T
  public int read_max_cost(
      InputStream in, byte[] buffer, int byteOffset, int byteCount, ArrayList<Integer> list)
      throws IOException {
    int maxBytesToRead = Math.min(byteCount, mBytesToRead);
    int bytesRead = in.read(buffer, byteOffset, maxBytesToRead);
    // after the join, we get maxBytesToRead in [0, +oo]. Hence, the loop gets T
    if (bytesRead > 0) {
      maxBytesToRead = bytesRead + 1;
    }
    for (int index = 0; index < maxBytesToRead; ++index) {}
    return 0;
  }

  private static void loop_over_charArray(StringBuilder builder, String input) {
    for (Character c : input.toCharArray()) {}
  }

  // We can't instantiate loop_over_charArray properly because I don't
  // know what to do with the symbol for the call to "toCharArray()"
  private static void call_loop_over_charArray_FN(StringBuilder out, String in) {
    loop_over_charArray(out, in);
  }
}
