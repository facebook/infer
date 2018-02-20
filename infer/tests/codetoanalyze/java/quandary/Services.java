/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.quandary;

import java.io.IOException;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.Runtime;

class Services {

}

// classes annotated with @ThriftService are servers (sources), whereas interfaces
// annotated with @ThriftService are clients (sinks): see
// https://github.com/facebook/swift/blob/master/swift-service/README.md#clients-and-servers
@Retention(RetentionPolicy.CLASS)
@interface ThriftService {
}


@ThriftService
class Service1 {

  public void serviceMethodBad(String s) throws IOException {
    Runtime.getRuntime().exec(s); // RCE if s is tainted, we should warn
  }

  // assume protected methods aren't exported to Thrift
  protected void protectedServiceMethodOk(String s) throws IOException {
    Runtime.getRuntime().exec(s);
  }

  // assume package-protected methods aren't exported to Thrift
  void packageProtectedServiceMethodOk(String s) throws IOException {
    Runtime.getRuntime().exec(s);
  }

  // private methods can't be exported to thrift
  private void privateMethodNotEndpointOk(String s) throws IOException {
    Runtime.getRuntime().exec(s);
  }

}

@ThriftService
interface ThriftInterface {

  public void interfaceServiceMethodBad(String s) throws IOException;
}

// this is a service too
class Implementer implements ThriftInterface {

  public void interfaceServiceMethodBad(String s) throws IOException {
    Runtime.getRuntime().exec(s); // RCE if s is tainted, we should warn
  }

}
