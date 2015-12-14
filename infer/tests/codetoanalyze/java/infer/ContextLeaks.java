/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.infer;

import android.content.Context;
import android.app.Activity;
import android.os.Handler;


public class ContextLeaks extends Activity {

  static Object sFld;

  void directLeak() {
    sFld = this;
  }

  public void leakThenFix() {
    sFld = this;
    sFld = null;
  }

  public void nonActivityNoLeak() {
    sFld = new Object();
  }

  static class Obj {
    public Object f;
  }

  public void indirectLeak() {
    Obj o = new Obj();
    o.f = this;
    sFld = o;
  }

  public void indirectLeakThenFix() {
    Obj o = new Obj();
    o.f = this;
    sFld = o;
    o.f = null;
  }

  class NonStaticInner {
  }

  public void nonStaticInnerClassLeak() {
    sFld = new NonStaticInner();
  }

  public void nonStaticInnerClassLeakThenFix() {
    sFld = new NonStaticInner();
    sFld = null;
  }

  private Object o;

  public void leakAfterInstanceFieldWrite() {
    this.o = new Object();
    sFld = this;
  }

  public static class Singleton {

    private static Singleton instance;
    private Context context;

    private Singleton(Context context) {
      this.context = context;
    }

    public static Singleton getInstance(Context context) {
      if (instance == null) {
        instance = new Singleton(context);
      }
      return instance;
    }
  }

  public Singleton singletonLeak() {
    return Singleton.getInstance(this);
  }

  public Singleton singletonNoLeak() {
    return Singleton.getInstance(this.getApplicationContext());
  }

  private Handler handler = new Handler();

  public void indirectHandlerLeak() {
    handlerLeak();
  }

  private void handlerLeak() {
    Runnable r =
        new Runnable() {
          public void run() {
          }
        };
    handler.postDelayed(r, 10000);
  }

  public void handlerNoLeak() {
    Runnable r =
        new Runnable() {
          public void run() {
          }
        };
    handler.postDelayed(r, 10000);
    handler.removeCallbacks(r);
  }

}
