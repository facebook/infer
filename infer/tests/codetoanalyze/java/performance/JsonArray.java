/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package libraries.marauder.analytics.utils.json;

public class JsonArray implements JsonType {

  public StringBuilder array = new StringBuilder("[");

  public void addStringEntry(String value) {
    if (array.length() != 1) {
      array.append(",");
    }
    JsonUtils.serialize(array, value);
  }
}
