package codetoanalyze.java.harness;

import android.app.Activity;

/**
 * If my subclasses null out mObj in an earlier lifecycle method, it will cause
 * a NPE in onDestroy.
 */
public class SuperclassActivity extends Activity {

  Object mObj = new Object();

  @Override
  public void onDestroy() {
    super.onDestroy();
    mObj.toString();
  }
}
