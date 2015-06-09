package codetoanalyze.java.harness;

public class SubclassActivity extends SuperclassActivity {

  @Override
  public void onPause() {
    super.onPause();
    mObj = null;
  }
}
