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

import android.content.ContentValues;
import android.content.SharedPreferences;

import com.facebook.infer.models.InferTaint;
import com.facebook.infer.annotation.PrivacySource;
import com.facebook.infer.annotation.PrivacySink;

public class TaintExample {

  public InputStream socketNotVerifiedSimple(SSLSocketFactory f)
    throws IOException {
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

  public InputStream taintingShouldNotPreventInference1(SSLSocketFactory f) throws IOException {
    socketNotVerifiedSimple(f).toString();
    // failing to infer a post for socketNotVerifiedSimple will hide this error
    Socket s = f.createSocket();
    return s.getInputStream();
  }

  public InputStream readInputStream(Socket socket) throws IOException {
    return socket.getInputStream();
  }

  // if we're not careful, postcondition inference will fail for this function
  Socket callReadInputStreamCauseTaintError(SSLSocketFactory f)
    throws IOException {
    Socket socket = f.createSocket();
    InputStream s = readInputStream(socket);
    s.toString(); // to avoid RETURN_VALUE_IGNORED warning
    return f.createSocket();
  }

  InputStream taintingShouldNotPreventInference2(SSLSocketFactory f) throws IOException {
    // if inference fails for this callee, we won't report an error here
    Socket s = callReadInputStreamCauseTaintError(f);
    return s.getInputStream();
  }

  public void simpleTaintErrorWithModelMethods() {
    Object o = InferTaint.inferSecretSource();
    InferTaint.inferSensitiveSink(o);
  }

  public Object returnTaintedSourceModelMethods() {
    return InferTaint.inferSecretSource();
  }

  public void callSinkMethodModelMethods(Object o) {
    InferTaint.inferSensitiveSink(o);
  }

  public void interprocTaintErrorWithModelMethods1() {
    InferTaint.inferSensitiveSink(returnTaintedSourceModelMethods());
  }

  public void interprocTaintErrorWithModelMethods2() {
    callSinkMethodModelMethods(InferTaint.inferSecretSource());
  }

  public void interprocTaintErrorWithModelMethods3() {
    callSinkMethodModelMethods(returnTaintedSourceModelMethods());
  }

  public void simpleTaintErrorWithModelMethodsUndefined() {
    Object o = InferTaint.inferSecretSourceUndefined();
    InferTaint.inferSensitiveSinkUndefined(o);
  }

  public Object returnTaintedSourceModelMethodsUndefined() {
    return InferTaint.inferSecretSourceUndefined();
  }

  public void callSinkMethodModelMethodsUndefined(Object o) {
    InferTaint.inferSensitiveSinkUndefined(o);
  }

  public void interprocTaintErrorWithModelMethodsUndefined1() {
    InferTaint.inferSensitiveSinkUndefined(returnTaintedSourceModelMethodsUndefined());
  }

  public void interprocTaintErrorWithModelMethodsUndefined2() {
    callSinkMethodModelMethodsUndefined(InferTaint.inferSecretSourceUndefined());
  }

  public void interprocTaintErrorWithModelMethodsUndefined3() {
    callSinkMethodModelMethodsUndefined(returnTaintedSourceModelMethodsUndefined());
  }

  public void contentValuesPutWithTaintedString(ContentValues values, SharedPreferences prefs,
                                                String key, String value) {
    values.put(key, prefs.getString(key, value));
  }

  public void contentValuesPutOk(ContentValues values, String key, String value) {
    values.put(key, value);
  }

  @PrivacySource("")
  public String privacySource() {
    return "source";
  }

  public void testPrivacySourceAnnot() {
    InferTaint.inferSensitiveSinkUndefined(privacySource()); // should report
  }

  public void instancePrivacySink(@PrivacySink("") String s1, String s2) {
  }

  public static void staticPrivacySink(@PrivacySink("") String s1, String s2) {
  }

  public void testPrivacySinkAnnot1() {
    String source = privacySource();
    instancePrivacySink(source, ""); // should report
  }

  public void testPrivacySinkAnnot2() {
    String source = privacySource();
    instancePrivacySink("", source); // should not report
  }

  public void testPrivacySinkAnnot3() {
    String source = privacySource();
    staticPrivacySink(source, ""); // should report
  }

  public void testPrivacySinkAnnot4() {
    String source = privacySource();
    staticPrivacySink("", source); // should not report
  }

}
