# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# you have to set MODELS_DIR to infer/models/ before including this file

# We want infer to still be able to override PATH so that it can capture compilation
# commands. Makefile.config will override PATH to be the configure-time one, which defeats
# infer.
#
# When called from our models Makefile PATH will be set by Makefile.config already, then infer will
# add its wrappers, so we can trust the original PATH here.
#
# Save the original PATH, include Makefile.config, and use the original PATH.
ORIGPATH = $(shell printf "%s" "$$PATH")
ROOT_DIR = $(MODELS_DIR)/../..
include $(ROOT_DIR)/Makefile.config
export PATH := $(ORIGPATH)
