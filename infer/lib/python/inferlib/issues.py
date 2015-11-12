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

import codecs
import csv
import json
import os
import shutil
import sys
import tempfile
import xml.etree.ElementTree as ET

from . import config, utils


# Increase the limit of the CSV parser to sys.maxlimit
csv.field_size_limit(sys.maxsize)

ISSUE_KIND_ERROR = 'ERROR'
ISSUE_KIND_WARNING = 'WARNING'
ISSUE_KIND_INFO = 'INFO'

ISSUE_TYPES = [
    'ASSERTION_FAILURE',
    'BAD_POINTER_COMPARISON',
    # 'CHECKERS_PRINTF_ARGS'
    # TODO (#8030397): revert this once all the checkers are moved to Infer
    'CONTEXT_LEAK',
    'MEMORY_LEAK',
    'RESOURCE_LEAK',
    'RETAIN_CYCLE',
    'STRONG_DELEGATE_WARNING',
    'TAINTED_VALUE_REACHING_SENSITIVE_FUNCTION',
    'IVAR_NOT_NULL_CHECKED',
    'NULL_DEREFERENCE',
    'PARAMETER_NOT_NULL_CHECKED',
    'PREMATURE_NIL_TERMINATION_ARGUMENT',
]

NULL_STYLE_ISSUE_TYPES = [
    'IVAR_NOT_NULL_CHECKED',
    'NULL_DEREFERENCE',
    'PARAMETER_NOT_NULL_CHECKED',
    'PREMATURE_NIL_TERMINATION_ARGUMENT',
]


def clean_csv(args, csv_report):
    collected_rows = []
    with open(csv_report, 'r') as file_in:
        reader = csv.reader(file_in)
        rows = [row for row in reader]
        if len(rows) <= 1:
            return rows
        else:
            for row in rows[1:]:
                filename = row[utils.CSV_INDEX_FILENAME]
                if os.path.isfile(filename):
                    if args.no_filtering \
                       or _should_report_csv(args.analyzer, row):
                        collected_rows.append(row)
            collected_rows = sorted(
                collected_rows,
                cmp=_compare_csv_rows)
            collected_rows = [rows[0]] + collected_rows
    temporary_file = tempfile.mktemp()
    with open(temporary_file, 'w') as file_out:
        writer = csv.writer(file_out)
        writer.writerows(collected_rows)
        file_out.flush()
        shutil.move(temporary_file, csv_report)


def clean_json(args, json_report):
    collected_rows = []
    with open(json_report, 'r') as file_in:
        rows = json.load(file_in)
        for row in rows:
            filename = row[utils.JSON_INDEX_FILENAME]
            if os.path.isfile(filename):
                if args.no_filtering \
                   or _should_report_json(args.analyzer, row):
                    collected_rows.append(row)
        collected_rows = sorted(
            collected_rows,
            cmp=_compare_json_rows)
    temporary_file = tempfile.mktemp()
    with open(temporary_file, 'w') as file_out:
        json.dump(collected_rows, file_out)
        file_out.flush()
        shutil.move(temporary_file, json_report)


def print_errors(json_report, bugs_out):
    with codecs.open(json_report, 'r', encoding=utils.LOCALE) as file_in:
        errors = json.load(file_in)

        errors = filter(lambda row: row[utils.JSON_INDEX_KIND] in
                        [ISSUE_KIND_ERROR, ISSUE_KIND_WARNING],
                        errors)

        with codecs.open(bugs_out, 'w', encoding=utils.LOCALE) as file_out:
            text_errors_list = []
            for row in errors:
                filename = row[utils.JSON_INDEX_FILENAME]
                if os.path.isfile(filename):
                    kind = row[utils.JSON_INDEX_KIND]
                    line = row[utils.JSON_INDEX_LINE]
                    error_type = row[utils.JSON_INDEX_TYPE]
                    msg = row[utils.JSON_INDEX_QUALIFIER]
                    indenter = utils.Indenter()
                    indenter.indent_push()
                    indenter.add(
                        utils.build_source_context(filename,
                                                   utils.TERMINAL_FORMATTER,
                                                   int(line)))
                    source_context = unicode(indenter)
                    text_errors_list.append(
                        u'{0}:{1}: {2}: {3}\n  {4}\n{5}'.format(
                            filename,
                            line,
                            kind.lower(),
                            error_type,
                            msg,
                            source_context,
                        )
                    )
            n_issues = len(text_errors_list)
            if n_issues == 0:
                _print_and_write(file_out, 'No issues found')
            else:
                msg = '\nFound %s\n' % utils.get_plural('issue', n_issues)
                _print_and_write(file_out, msg)
                text_errors = '\n\n'.join(text_errors_list)
                _print_and_write(file_out, text_errors)


def _compare_issues(filename_1, line_1, filename_2, line_2):
    if filename_1 < filename_2:
        return -1
    elif filename_1 > filename_2:
        return 1
    else:
        return line_1 - line_2


def _compare_csv_rows(row_1, row_2):
    filename_1 = row_1[utils.CSV_INDEX_FILENAME]
    filename_2 = row_2[utils.CSV_INDEX_FILENAME]
    line_1 = int(row_1[utils.CSV_INDEX_LINE])
    line_2 = int(row_2[utils.CSV_INDEX_LINE])
    return _compare_issues(filename_1, line_1, filename_2, line_2)


def _compare_json_rows(row_1, row_2):
    filename_1 = row_1[utils.JSON_INDEX_FILENAME]
    filename_2 = row_2[utils.JSON_INDEX_FILENAME]
    line_1 = row_1[utils.JSON_INDEX_LINE]
    line_2 = row_2[utils.JSON_INDEX_LINE]
    return _compare_issues(filename_1, line_1, filename_2, line_2)


def _should_report(analyzer, error_kind, error_type, error_bucket):
    analyzers_whitelist = [
        config.ANALYZER_ERADICATE,
        config.ANALYZER_CHECKERS,
        config.ANALYZER_TRACING,
    ]
    error_kinds = [ISSUE_KIND_ERROR, ISSUE_KIND_WARNING]
    null_style_buckets = ['B1', 'B2']

    if analyzer in analyzers_whitelist:
        return True

    if error_kind not in error_kinds:
        return False

    if not error_type:
        return False

    if error_type in NULL_STYLE_ISSUE_TYPES:
        return error_bucket in null_style_buckets

    return error_type in ISSUE_TYPES


def _should_report_csv(analyzer, row):
    error_kind = row[utils.CSV_INDEX_KIND]
    error_type = row[utils.CSV_INDEX_TYPE]
    error_bucket = ''  # can be updated later once we extract it from qualifier

    try:
        qualifier_xml = ET.fromstring(row[utils.CSV_INDEX_QUALIFIER_TAGS])
        if qualifier_xml.tag == utils.QUALIFIER_TAGS:
            bucket = qualifier_xml.find(utils.BUCKET_TAGS)
            if bucket is not None:
                error_bucket = bucket.text
    except ET.ParseError:
        pass  # this will skip any invalid xmls

    return _should_report(analyzer, error_kind, error_type, error_bucket)


def _should_report_json(analyzer, row):
    error_kind = row[utils.JSON_INDEX_KIND]
    error_type = row[utils.JSON_INDEX_TYPE]
    error_bucket = ''  # can be updated later once we extract it from qualifier

    for qual_tag in row[utils.QUALIFIER_TAGS]:
        if qual_tag['tag'] == utils.BUCKET_TAGS:
            error_bucket = qual_tag['value']
            break

    return _should_report(analyzer, error_kind, error_type, error_bucket)


def _print_and_write(file_out, message):
    print(message)
    file_out.write(message + '\n')
