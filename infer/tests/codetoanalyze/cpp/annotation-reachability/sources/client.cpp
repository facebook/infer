/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include "../forbidden/library.h"

namespace client {

using namespace library;

void call_protected_api_bad() { details::low_level(); }

void call_wrapper_ok() { safewrapper::wrapper(); }

struct CallWrapperOk : public safewrapper::Wrapper {
  CallWrapperOk() {}
  ~CallWrapperOk() {}
};

struct CallLowLevelBad {
  details::LowLevel wrapped;

  CallLowLevelBad() {}
  ~CallLowLevelBad() {}
};

} // namespace client
