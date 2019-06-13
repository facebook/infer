/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package infer.inferandroidexample;

import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;
import android.view.Menu;
import android.view.MenuItem;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Calendar;

public class MainActivity extends ActionBarActivity {

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_main);
    String s = getDay();
    int length = s.length();
    writeToFile();
  }

  private String getDay() {
    if (Calendar.getInstance().get(Calendar.DAY_OF_WEEK) == Calendar.WEDNESDAY) {
      return "Wednesday";
    } else {
      return otherOutput();
    }
  }

  private String otherOutput() {
    return null;
  }

  private void writeToFile() {
    byte[] arr = {1, 2, 3};
    FileOutputStream fis;
    try {
      fis = new FileOutputStream("file.txt");
      fis.write(arr);
      fis.close();
    } catch (IOException e) {
      // Deal with exception
    }
  }

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    // Inflate the menu; this adds items to the action bar if it is present.
    getMenuInflater().inflate(R.menu.menu_main, menu);
    return true;
  }

  @Override
  public boolean onOptionsItemSelected(MenuItem item) {
    // Handle action bar item clicks here. The action bar will
    // automatically handle clicks on the Home/Up button, so long
    // as you specify a parent activity in AndroidManifest.xml.
    int id = item.getItemId();

    //noinspection SimplifiableIfStatement
    if (id == R.id.action_settings) {
      return true;
    }

    return super.onOptionsItemSelected(item);
  }

  private void inferShouldNotReport() {
    // Generated.java is supposed to be skipped by infer, thus even though
    // Generated.returnsNull() returns null, infer is not supposed to know
    // about it hence should not report an NPE here
    Object o = Generated.returnsNull();
    o.toString();
  }
}
