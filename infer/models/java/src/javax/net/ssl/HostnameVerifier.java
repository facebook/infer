/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package javax.net.ssl;

import java.net.Socket;
import javax.net.SocketFactory;
import javax.net.ssl.SSLSession;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;

public class HostnameVerifier {

  public boolean verify(
      String hostname,
      SSLSession session) {

    Socket socket = SocketFactory.getLastSocket();

    if (InferUndefined.boolean_undefined()) {
      // verification succeeded; we can untaint the socket
      InferBuiltins.__set_untaint_attribute(socket);
      return true;
    } else {
      // verification failed; we can't untaint the socket
      return false;
    }
  }


}
