package codetoanalyze.java.harness;

import android.app.Activity;
import android.os.Bundle;

/*
 * Test if harness generation understands basics of Activity lifecycle.
 */
public class BasicHarnessActivity extends Activity {

  public BasicHarnessActivity(BasicHarnessActivity a) {
  }

  private Object mObj;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    mObj = new Object();
  }

  @Override
  public void onPause() {
    mObj = null;
  }

  @Override
  public void onDestroy() {
    String s = mObj.toString();
  }

}
