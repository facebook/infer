/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;

// A small administration server listening on a TCP port. Whatever is read from a connection comes
// from the peer at the other end of it.
public class SocketInput {

  private ServerSocket server;

  Object receiveObjectBad() throws IOException, ClassNotFoundException {
    try (Socket client = server.accept();
        ObjectInputStream in = new ObjectInputStream(client.getInputStream())) {
      return in.readObject();
    }
  }

  void runCommandBad() throws IOException {
    try (Socket client = server.accept()) {
      String command = new String(client.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
      Runtime.getRuntime().exec(command);
    }
  }

  // the response of a remote service is not trusted either
  void runRemoteCommandBad(String host) throws IOException {
    try (Socket socket = new Socket(host, 7000)) {
      String command = new String(socket.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
      Runtime.getRuntime().exec(command);
    }
  }

  // writing to the connection is not a source
  void statusOk() throws IOException {
    try (Socket client = server.accept()) {
      OutputStream out = client.getOutputStream();
      Process uptime = Runtime.getRuntime().exec("uptime");
      out.write(uptime.getInputStream().readAllBytes());
    }
  }

  // False negatives: the usual way to read from a socket is to wrap its stream in a reader or a
  // filter stream. Pulse models the constructors of these wrappers to track resources (see
  // `Resource.allocate_with_delegation` in PulseModelsJava.ml), and the model does not propagate
  // the taint of the wrapped stream to the wrapper. The methods reading from the wrapper then
  // propagate the taint of their receiver, which has none. Taint propagators in the taint config
  // do not help, as they do not apply to modeled calls. Reading the stream directly, as above, is
  // reported.

  // the taint is lost in `new InputStreamReader(...)` and `new BufferedReader(...)`, before
  // `readLine()`
  void FN_runCommandLineBad() throws IOException {
    try (Socket client = server.accept();
        BufferedReader in =
            new BufferedReader(
                new InputStreamReader(client.getInputStream(), StandardCharsets.UTF_8))) {
      Runtime.getRuntime().exec(in.readLine());
    }
  }

  // the taint is lost in `new DataInputStream(...)`, before `readUTF()`
  File FN_uploadedFileBad() throws IOException {
    try (Socket client = server.accept();
        DataInputStream in = new DataInputStream(client.getInputStream())) {
      return new File("/var/uploads", in.readUTF());
    }
  }
}
