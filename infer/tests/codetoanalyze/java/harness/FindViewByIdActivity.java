package codetoanalyze.java.harness;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;


public class FindViewByIdActivity extends Activity {

  private MyView mView;

  @Override
  public void onCreate(Bundle b) {
    mView = (MyView) findViewById(-1);
    // replacing the above line with this reveals the bug
    // mView = new MyView(this.getApplicationContext()); 
    Button btn = new Button(this.getApplicationContext());
    Button.OnClickListener listener = new Button.OnClickListener() {

      @Override
      public void onClick(View v) {
        // oops! what if I get nulled out later?
        mView.toString();
      }
    };
    btn.setOnClickListener(listener);
  }


  @Override
  public void onDestroy() {
    mView = null;
  }
}
