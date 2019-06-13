/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package libraries.marauder.analytics.utils.json;

public class JsonString implements JsonType {

  public String array;

  public JsonString(String input) {
    array = JsonUtils.serialize(input).toString();
  }

  public JsonString(long input) {
    array = JsonUtils.serialize(input);
  }

  public JsonString(double input) {
    array = JsonUtils.serialize(input);
  }

  public JsonString(boolean input) {
    array = JsonUtils.serialize(input);
  }

  public String toString() {
    return array;
  }
}
