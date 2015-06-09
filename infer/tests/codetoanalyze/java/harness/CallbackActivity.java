package codetoanalyze.java.harness;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;

/*
 * Test if harness generation knows how to call a callback defined in an inner class
 */
public class CallbackActivity extends Activity {

  private Object mObj;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    Button btn = new Button(this.getApplicationContext());
    mObj = new Object();
    Button.OnClickListener listener = new Button.OnClickListener() {

      @Override
      public void onClick(View v) {
        // oops! what if I get nulled out later?
        mObj.toString();
      }
    };
    btn.setOnClickListener(listener);
    mObj = null;
  }

}
