# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import locale
import os


try:
    locale.setlocale(locale.LC_ALL, '')
    CODESET = locale.getlocale(locale.LC_CTYPE)[1]
except locale.Error:
    CODESET = None
if CODESET is None:
    CODESET = 'ascii'

# this assumes that this file lives in infer/lib/python/infer/ and the binaries
# are in infer/bin/
INFER_PYTHON_DIRECTORY = os.path.dirname(os.path.realpath(__file__)
                                         .decode(CODESET))
INFER_INFER_DIRECTORY = os.path.join(INFER_PYTHON_DIRECTORY,
                                     os.pardir, os.pardir, os.pardir)
INFER_ROOT_DIRECTORY = os.path.join(INFER_INFER_DIRECTORY, os.pardir)
FCP_DIRECTORY = os.path.join(INFER_ROOT_DIRECTORY, 'facebook-clang-plugins')
LIB_DIRECTORY = os.path.join(INFER_INFER_DIRECTORY, 'lib')
BIN_DIRECTORY = os.path.join(INFER_INFER_DIRECTORY, 'bin')
JAVA_LIB_DIRECTORY = os.path.join(LIB_DIRECTORY, 'java')
MODELS_JAR = os.path.join(JAVA_LIB_DIRECTORY, 'models.jar')
ANNOT_PROCESSOR_JAR = os.path.join(JAVA_LIB_DIRECTORY, 'processor.jar')
ANNOT_PROCESSOR_NAMES = \
    'com.facebook.infer.annotprocess.CollectSuppressWarnings'
WRAPPERS_DIRECTORY = os.path.join(LIB_DIRECTORY, 'wrappers')
XCODE_WRAPPERS_DIRECTORY = os.path.join(LIB_DIRECTORY, 'xcode_wrappers')

DEFAULT_INFER_OUT = os.path.join(os.getcwd().decode(CODESET), 'infer-out')
CSV_PERF_FILENAME = 'performances.csv'
STATS_FILENAME = 'stats.json'
PROC_STATS_FILENAME = 'proc_stats.json'

CSV_REPORT_FILENAME = 'report.csv'
JSON_REPORT_FILENAME = 'report.json'
INFER_BUCK_DEPS_FILENAME = 'infer-deps.txt'
BUGS_FILENAME = 'bugs.txt'
JAVAC_FILELISTS_FILENAME = 'filelists'
PMD_XML_FILENAME = 'report.xml'

IOS_CAPTURE_ERRORS = 'errors'
IOS_BUILD_OUTPUT = 'build_output'

LOG_FILE = 'toplevel.log'

BUCK_INFER_OUT = 'infer'

SUPRESS_WARNINGS_OUTPUT_FILENAME_OPTION = 'SuppressWarningsOutputFilename'


# list of possible analyzers
ANALYZER_INFER = 'infer'
ANALYZER_ERADICATE = 'eradicate'
ANALYZER_CHECKERS = 'checkers'
ANALYZER_CAPTURE = 'capture'
ANALYZER_COMPILE = 'compile'
ANALYZER_TRACING = 'tracing'
ANALYZER_CRASHCONTEXT = 'crashcontext'
ANALYZER_LINTERS = 'linters'
ANALYZER_QUANDARY = 'quandary'
ANALYZER_THREADSAFETY = 'threadsafety'

ANALYZERS = [
    ANALYZER_CAPTURE,
    ANALYZER_CHECKERS,
    ANALYZER_COMPILE,
    ANALYZER_CRASHCONTEXT,
    ANALYZER_ERADICATE,
    ANALYZER_INFER,
    ANALYZER_LINTERS,
    ANALYZER_TRACING,
    ANALYZER_QUANDARY,
    ANALYZER_THREADSAFETY,
]
