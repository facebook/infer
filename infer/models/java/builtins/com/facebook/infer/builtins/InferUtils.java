/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.infer.builtins;

public class InferUtils {

  public static boolean isValidCharset(String charsetName) {
    return charsetName == "UTF8"
        || charsetName == "utf8"
        || charsetName == "UTF-8"
        || charsetName == "utf-8"
        || charsetName == "US-ASCII"
        || charsetName == "us-ascii"
        || charsetName == "ISO-8859-1"
        || charsetName == "iso-8859-1"
        || charsetName == "UTF-16BE"
        || charsetName == "utf-16be"
        || charsetName == "UTF-16LE"
        || charsetName == "utf-16le"
        || charsetName == "UTF-16"
        || charsetName == "utf-16";
  }
}
