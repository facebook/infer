/*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

package codetoanalyze.java.infer;

import java.io.InputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.Socket;
import java.net.URL;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;

public class TaintExample {

  String test_equals(String s) {

    String my_string ="a string";
    String res;

    if (my_string.equals(s)) {
      res = "OK";
    }  else {
      res = "NOT OK";
    }
    return res;
  }

  String test_compareTo(String s) {

        String my_string ="a string";
        String res;

        if (my_string.compareTo(s) == 1) {
            res = "OK";
        }  else {
            res = "NOT OK";
        }
        return res;
    }

    String test_endsWith(String s) {

        String my_string ="a string";
        String res;

        if (my_string.endsWith(s)) {
            res = "OK";
        }  else {
            res = "NOT OK";
        }
        return res;
    }

    String test_startsWith(String s) {

        String my_string ="a string";
        String res;

        if (my_string.startsWith(s)) {
            res = "OK";
        }  else {
            res = "NOT OK";
        }
        return res;
    }


  public String taintGetHostEquals (String s) throws MalformedURLException {

    String res;
    URL u = new URL(s);
    String s1 = u.getHost();
    res = test_equals(s1);
    return res;
  }

    public String taintGetHostCompareTo (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.getHost();
        res = test_compareTo(s1);
        return res;

    }

    public String taintGetHostEndsWith (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.getHost();
        res = test_endsWith(s1);
        return res;
    }

    public String taintGetHostStartsWith (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.getHost();
        res = test_startsWith(s1);
        return res;
    }

    public String taintGetAuthoriyEquals (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.getAuthority();
        res = test_equals(s1);
        return res;
    }

    public String taintGetAuthorityCompareTo (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.getAuthority();
        res = test_compareTo(s1);
        return res;

    }

    public String taintGetAuthorityEndsWith (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.getAuthority();
        res = test_endsWith(s1);
        return res;
    }

    public String taintGetAuthorityStartsWith (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.getAuthority();
        res = test_startsWith(s1);
        return res;
    }

    public String taintGetProtocolEquals (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.getProtocol();
        res = test_equals(s1);
        return res;
    }

    public String taintGetProtocolCompareTo (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.getProtocol();
        res = test_compareTo(s1);
        return res;

    }

    public String taintGetProtocolEndsWith (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.getProtocol();
        res = test_endsWith(s1);
        return res;
    }

    public String taintGetProtocolStartsWith (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.getProtocol();
        res = test_startsWith(s1);
        return res;
    }

    public String taintToExternalFormEquals (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.toExternalForm();
        res = test_equals(s1);
        return res;
    }

    public String taintToExternalFormCompareTo (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.toExternalForm();
        res = test_compareTo(s1);
        return res;

    }

    public String taintToExternalFormEndsWith (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.toExternalForm();
        res = test_endsWith(s1);
        return res;
    }

    public String taintToExternalFormStartsWith (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.toExternalForm();
        res = test_startsWith(s1);
        return res;
    }

    public String taintToStringEquals (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.toString();
        res = test_equals(s1);
        return res;
    }

    public String taintToStringCompareTo (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.toString();
        res = test_compareTo(s1);
        return res;

    }

    public String taintToStringEndsWith (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.toString();
        res = test_endsWith(s1);
        return res;
    }

    public String taintToStringStartsWith (String s) throws MalformedURLException {

        String res;
        URL u = new URL(s);
        String s1 = u.toString();
        res = test_startsWith(s1);
        return res;
    }

  public InputStream socketNotVerifiedSimple(SSLSocketFactory f)
    throws IOException {
    Socket socket = f.createSocket();
    return socket.getInputStream();
  }

  public InputStream taintingShouldNotPreventInference(SSLSocketFactory f)
    throws IOException {

    socketNotVerifiedSimple(f).toString();
    // failing to infer a post for socketNotVerifiedSimple will hide this error
    Socket socket = f.createSocket();
    return socket.getInputStream();
  }

  public InputStream socketVerifiedForgotToCheckRetval(SSLSocketFactory f,
                                                       HostnameVerifier v,
                                                       SSLSession session)
    throws IOException {

    Socket socket = f.createSocket();
    v.verify("hostname", session);
    return socket.getInputStream();
  }

  public InputStream socketVerifiedOk1(SSLSocketFactory f,
                                       HostnameVerifier v,
                                       SSLSession session)
    throws IOException {

    Socket socket = f.createSocket();
    if (v.verify("hostname", session)) {
      return socket.getInputStream();
    } else {
      return null;
    }
  }


  HostnameVerifier mHostnameVerifier;

  public void throwExceptionIfNoVerify(SSLSocket sslSocket, String host)
    throws IOException {

    if (!mHostnameVerifier.verify(host, sslSocket.getSession())) {
      throw new SSLException("Couldn't verify!");
    }
  }

  public InputStream socketVerifiedOk2(SSLSocketFactory f) throws IOException {
    SSLSocket s = (SSLSocket) f.createSocket();
    throwExceptionIfNoVerify(s, "hostname");
    return s.getInputStream();
  }

  public InputStream socketIgnoreExceptionNoVerify(SSLSocketFactory f)
    throws IOException {

    SSLSocket s = (SSLSocket) f.createSocket();
    try {
      throwExceptionIfNoVerify(s, "hostname");
    } catch (SSLException e) {
      // ignore the fact that verifying the socket failed
    }
    return s.getInputStream();
  }


}
