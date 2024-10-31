/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void sink() {}

void not_si_nk() {}

void sanitizer() { sink(); }

void not_sani_tizer() { sink(); }

void source1_Ok() { not_si_nk(); }

void source2_Bad() { sink(); }

void source3_Ok() { sanitizer(); }

void source4_Bad() { not_sani_tizer(); }
