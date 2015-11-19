/*
* Copyright (c) 2015 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

package javax.net;

import java.net.Socket;

import com.facebook.infer.models.InferBuiltins;

public class SocketFactory {

  // using recency abstraction to remember the last Socket created
  private static Socket sLast;

  public static Socket getLastSocket() {
    return sLast;
  }

  // proxy for Socket of undefined type
  private native Socket genSocket();

  private Socket returnAllocatedSocket() {
    Socket socket = genSocket();
    InferBuiltins.assume_allocated(socket);
    sLast = socket;
    return socket;
  }

  public Socket createSocket() {
    Socket socket = returnAllocatedSocket();
    InferBuiltins.__set_taint_attribute(socket);
    return socket;
  }

}
