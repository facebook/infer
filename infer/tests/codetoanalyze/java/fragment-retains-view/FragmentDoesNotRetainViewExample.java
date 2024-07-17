/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import android.content.Context;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ListView;

public class FragmentDoesNotRetainViewExample extends Fragment {

  class CustomView extends ListView {

    public CustomView(Context c) {
      super(c);
    }
  }

  View mView1;
  View mView2;
  ViewGroup mViewSubclass;
  CustomView mCustomView;
  @AutoCleanup View mWillBeCleanedUp;

  boolean b;

  @Override
  public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle bundle) {
    mView1 = inflater.inflate(-1, container, false);
    mView2 = inflater.inflate(-1, container, false);
    mViewSubclass = (ViewGroup) inflater.inflate(-1, container, false);
    mCustomView = (CustomView) inflater.inflate(-1, container, false);
    mWillBeCleanedUp = inflater.inflate(-1, container, false);
    return container;
  }

  @Override
  public void onDestroyView() {
    mView1 = null;
    if (b) {
      mView2 = null; // conditional nulling is still ok
    }
    mCustomView = null;
    mViewSubclass = null;
  }
}
