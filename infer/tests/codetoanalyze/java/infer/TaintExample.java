/*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

package codetoanalyze.java.infer;


import java.net.MalformedURLException;
import java.net.URL;

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

}
