package codetoanalyze.java.infer;

import android.content.Context;
import android.app.Activity;

public class MainActivity extends Activity {

    //private static MainActivity activity;
    //private static Object inner;
    //private static View view;
    private static Context context;

    void setStaticActivity() {
        context = this;
    }
}
