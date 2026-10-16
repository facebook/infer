/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.net.URI;
import java.util.Set;

// Validators of an application: each one returns its argument when it is valid, and throws
// otherwise. The taint config of these tests declares them as sanitizers of kind `Sanitizer`, as a
// project would do for its own validators. Unlike a boolean check, a validator that throws cannot
// be ignored by mistake: once it returns, the value is valid.
class Validators {

  private static final Set<String> ALLOWED_HOSTS = Set.of("api.example.com");

  static String requireAlphanumeric(String value) {
    if (!value.matches("[A-Za-z0-9]+")) {
      throw new IllegalArgumentException("not alphanumeric: " + value);
    }
    return value;
  }

  static String requireFileName(String value) {
    if (value.isEmpty()
        || value.contains("/")
        || value.contains("\\")
        || value.equals(".")
        || value.equals("..")) {
      throw new IllegalArgumentException("not a file name: " + value);
    }
    return value;
  }

  static URI requireAllowedHost(URI uri) {
    if (!ALLOWED_HOSTS.contains(uri.getHost())) {
      throw new SecurityException("host not allowed: " + uri.getHost());
    }
    return uri;
  }
}
