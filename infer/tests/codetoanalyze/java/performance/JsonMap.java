/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package libraries.marauder.analytics.utils.json;

public class JsonMap implements JsonType {

  public StringBuilder map = new StringBuilder("{");

  public void addEntry(String key, JsonType value) {
    if (!JsonUtils.isValidInputType(value)) {
      // throw new IllegalStateException("illegal input type " + value);
    }

    addKeyToMap(key);
    map.append(value.toString());
  }

  public void addEntry(String key, String value) {
    addKeyToMap(key);
    JsonUtils.serialize(map, value);
  }

  public void addEntry(String key, long value) {
    addKeyToMap(key);
    JsonUtils.serialize(map, value);
  }

  public void addEntry(String key, double value) {
    addKeyToMap(key);
    JsonUtils.serialize(map, value);
  }

  public void addEntry(String key, boolean value) {
    addKeyToMap(key);
    JsonUtils.serialize(map, value);
  }

  public void addEntry(String key, Object value) {
    addKeyToMap(key);
    JsonUtils.serialize(map, value);
  }

  private void addKeyToMap(String key) {
    if (map.length() != 1) {
      map.append(",");
    }

    JsonUtils.serialize(map, key);
    map.append(":");
  }
}
