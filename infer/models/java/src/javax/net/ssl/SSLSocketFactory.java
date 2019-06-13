/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package javax.net.ssl;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import javax.net.SocketFactory;

public class SSLSocketFactory extends SocketFactory {

  public Socket createSocket(InetAddress addr, int i) throws IOException {
    return super.createSocket();
  }

  @Override
  public Socket createSocket(InetAddress addr1, int i, InetAddress addr2, int j)
      throws IOException {
    return super.createSocket();
  }

  @Override
  public Socket createSocket(String s, int i) throws IOException {
    return super.createSocket();
  }

  @Override
  public Socket createSocket(String s, int i, InetAddress addr, int j) throws IOException {
    return super.createSocket();
  }

  // the method below is an abstract method in the actual Java class, but we need to implement it
  // explicitly due to Infer's dynamic dispatch woes
  public Socket createSocket(Socket s, String host, int i, boolean b) throws IOException {
    return super.createSocket();
  }
}
