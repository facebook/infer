/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package codetoanalyze.java.infer;


public class ReturnValueIgnored {

  private int m() {
    return 1;
  }

  public void returnValueIgnored() {
    m();
  }

}

