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

public class FragmentRetainsViewExample extends Fragment {

  class CustomView extends ListView {

    public CustomView(Context c) {
      super(c);
    }
  }

  View mView;
  ViewGroup mViewSubclass;
  CustomView mCustomView;
  ReallyCustomStub<View> mReallyCustomView;

  @Override
  public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle bundle) {
    mView = inflater.inflate(-1, container, false);
    mViewSubclass = (ViewGroup) inflater.inflate(-1, container, false);
    mCustomView = (CustomView) inflater.inflate(-1, container, false);
    mReallyCustomView = new ReallyCustomStub(mCustomView);
    return container;
  }

  @Override
  public void onDestroyView() {
    // not nulling out anything
  }
}

/* Sometimes people implement a wrapper around a View without implementing the same interface.
 * For the purposes of memory leaks, though, we want to catch such cases too. That is why the
 * option --android-view-class-list allows one to add classes such as `ReallyCustomStub` to be
 * considered "views". */
class ReallyCustomStub<T extends View> {
  ReallyCustomStub(T view) {
    this.view = view;
  }

  T view;
  // .. some custom API here ...
}
