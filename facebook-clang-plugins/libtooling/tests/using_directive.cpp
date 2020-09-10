/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace nsa {
namespace nsb {
int a;
}
} // namespace nsa
using namespace nsa::nsb;
namespace B = nsa::nsb;
int b = a;
