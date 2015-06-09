package codetoanalyze.java.eradicate;

import android.app.Activity;
import android.os.Bundle;

public class ActivityFieldNotInitialized {

  class BadActivityWithOnCreate extends Activity {

    private String field;

    protected void onCreate(Bundle bundle) {
    }

  }

}
