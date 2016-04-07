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
import datetime
import itertools
import json
import operator
import os
import re
import shutil
import sys
import tempfile
import xml.etree.ElementTree as ET

try:
    from lxml import etree
except ImportError:
    etree = None

from . import colorize, config, source, utils


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
    'DIRECT_ATOMIC_PROPERTY_ACCESS',
    'CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK',
    'REGISTERED_OBSERVER_BEING_DEALLOCATED',
    'ASSIGN_POINTER_WARNING',
]

NULL_STYLE_ISSUE_TYPES = [
    'IVAR_NOT_NULL_CHECKED',
    'NULL_DEREFERENCE',
    'PARAMETER_NOT_NULL_CHECKED',
    'PREMATURE_NIL_TERMINATION_ARGUMENT',
]

# indices in rows of csv reports
CSV_INDEX_CLASS = 0
CSV_INDEX_KIND = 1
CSV_INDEX_TYPE = 2
CSV_INDEX_QUALIFIER = 3
CSV_INDEX_SEVERITY = 4
CSV_INDEX_LINE = 5
CSV_INDEX_PROCEDURE = 6
CSV_INDEX_PROCEDURE_ID = 7
CSV_INDEX_FILENAME = 8
CSV_INDEX_TRACE = 9
CSV_INDEX_KEY = 10
CSV_INDEX_QUALIFIER_TAGS = 11
CSV_INDEX_HASH = 12
CSV_INDEX_BUG_ID = 13
CSV_INDEX_ALWAYS_REPORT = 14
CSV_INDEX_ADVICE = 15

# field names in rows of json reports
JSON_INDEX_FILENAME = 'file'
JSON_INDEX_HASH = 'hash'
JSON_INDEX_KIND = 'kind'
JSON_INDEX_LINE = 'line'
JSON_INDEX_PROCEDURE = 'procedure'
JSON_INDEX_PROCEDURE_ID = 'procedure_id'
JSON_INDEX_QUALIFIER = 'qualifier'
JSON_INDEX_QUALIFIER_TAGS = 'qualifier_tags'
JSON_INDEX_SEVERITY = 'file'
JSON_INDEX_TYPE = 'bug_type'
JSON_INDEX_TRACE = 'bug_trace'
JSON_INDEX_TRACE_LEVEL = 'level'
JSON_INDEX_TRACE_FILENAME = 'filename'
JSON_INDEX_TRACE_LINE = 'line_number'
JSON_INDEX_TRACE_DESCRIPTION = 'description'
JSON_INDEX_TRACE_NODE_TAGS = 'node_tags'
JSON_INDEX_TRACE_NODE_TAGS_TAG = 'tags'
JSON_INDEX_TRACE_NODE_TAGS_VALUE = 'value'
JSON_INDEX_INFER_SOURCE_LOC = 'infer_source_loc'
JSON_INDEX_ISL_FILE = 'file'
JSON_INDEX_ISL_LNUM = 'lnum'
JSON_INDEX_ISL_CNUM = 'cnum'
JSON_INDEX_ISL_ENUM = 'enum'


QUALIFIER_TAGS = 'qualifier_tags'
BUCKET_TAGS = 'bucket'
ISSUE_TYPES_URL = 'http://fbinfer.com/docs/infer-issue-types.html#'


def clean_csv(args, csv_report):
    collected_rows = []
    with open(csv_report, 'r') as file_in:
        reader = csv.reader(file_in)
        rows = [row for row in reader]
        if len(rows) <= 1:
            return rows
        else:
            for row in rows[1:]:
                filename = row[CSV_INDEX_FILENAME]
                if os.path.isfile(filename):
                    if args.no_filtering \
                       or _should_report_csv(args.analyzer, row):
                        collected_rows.append(row)
            collected_rows.sort(key=operator.itemgetter(CSV_INDEX_FILENAME,
                                                        CSV_INDEX_LINE))
            collected_rows = [rows[0]] + collected_rows
    temporary_file = tempfile.mktemp()
    with open(temporary_file, 'w') as file_out:
        writer = csv.writer(file_out)
        writer.writerows(collected_rows)
        file_out.flush()
        shutil.move(temporary_file, csv_report)


def clean_json(args, json_report):
    rows = utils.load_json_from_path(json_report)

    def is_clean(row):
        filename = row[JSON_INDEX_FILENAME]
        return (os.path.isfile(filename) and
                (args.no_filtering or
                 _should_report_json(args.analyzer, row)))

    rows = filter(is_clean, rows)
    rows.sort(key=operator.itemgetter(JSON_INDEX_FILENAME,
                                      JSON_INDEX_LINE))
    temporary_file = tempfile.mktemp()
    utils.dump_json_to_path(rows, temporary_file)
    shutil.move(temporary_file, json_report)

def _text_of_infer_loc(loc):
    return ' ({}:{}:{}-{}:)'.format(
        loc[JSON_INDEX_ISL_FILE],
        loc[JSON_INDEX_ISL_LNUM],
        loc[JSON_INDEX_ISL_CNUM],
        loc[JSON_INDEX_ISL_ENUM],
    )


def text_of_report(report):
    filename = report[JSON_INDEX_FILENAME]
    kind = report[JSON_INDEX_KIND]
    line = report[JSON_INDEX_LINE]
    error_type = report[JSON_INDEX_TYPE]
    msg = report[JSON_INDEX_QUALIFIER]
    infer_loc = ''
    if JSON_INDEX_INFER_SOURCE_LOC in report:
        infer_loc = _text_of_infer_loc(report[JSON_INDEX_INFER_SOURCE_LOC])
    return '%s:%d: %s: %s%s\n  %s' % (
        filename,
        line,
        kind.lower(),
        error_type,
        infer_loc,
        msg,
    )


def _text_of_report_list(reports, formatter=colorize.TERMINAL_FORMATTER):
    text_errors_list = []
    error_types_count = {}
    for report in reports:
        filename = report[JSON_INDEX_FILENAME]
        line = report[JSON_INDEX_LINE]

        source_context = ''
        if formatter == colorize.TERMINAL_FORMATTER:
            source_context = source.build_source_context(
                filename,
                formatter,
                line,
            )
            indenter = source.Indenter() \
                             .indent_push() \
                             .add(source_context)
            source_context = '\n' + unicode(indenter)

        msg = text_of_report(report)
        if report[JSON_INDEX_KIND] == ISSUE_KIND_ERROR:
            msg = colorize.color(msg, colorize.ERROR, formatter)
        elif report[JSON_INDEX_KIND] == ISSUE_KIND_WARNING:
            msg = colorize.color(msg, colorize.WARNING, formatter)
        text = '%s%s' % (msg, source_context)
        text_errors_list.append(text)

        t = report[JSON_INDEX_TYPE]
        # assert failures are not very informative without knowing
        # which assertion failed
        if t == 'Assert_failure' and JSON_INDEX_INFER_SOURCE_LOC in report:
            t += _text_of_infer_loc(report[JSON_INDEX_INFER_SOURCE_LOC])
        if t not in error_types_count:
            error_types_count[t] = 1
        else:
            error_types_count[t] += 1

    n_issues = len(text_errors_list)
    if n_issues == 0:
        if formatter == colorize.TERMINAL_FORMATTER:
            out = colorize.color('  No issues found  ',
                                 colorize.SUCCESS, formatter)
            return out + '\n'
        else:
            return 'No issues found'

    max_type_length = max(map(len, error_types_count.keys())) + 2
    sorted_error_types = error_types_count.items()
    sorted_error_types.sort(key=operator.itemgetter(1), reverse=True)
    types_text_list = map(lambda (t, count): '%s: %d' % (
        t.rjust(max_type_length),
        count,
    ), sorted_error_types)

    text_errors = '\n\n'.join(text_errors_list)

    issues_found = 'Found {n_issues}'.format(
        n_issues=utils.get_plural('issue', n_issues),
    )
    msg = '{issues_found}\n\n{issues}\n\n{header}\n\n{summary}'.format(
        issues_found=colorize.color(issues_found,
                                    colorize.HEADER,
                                    formatter),
        issues=text_errors,
        header=colorize.color('Summary of the reports',
                              colorize.HEADER, formatter),
        summary='\n'.join(types_text_list),
    )
    return msg


def _is_user_visible(report):
    filename = report[JSON_INDEX_FILENAME]
    kind = report[JSON_INDEX_KIND]
    return (os.path.isfile(filename) and
            kind in [ISSUE_KIND_ERROR, ISSUE_KIND_WARNING])


def print_and_save_errors(json_report, bugs_out, xml_out):
    errors = utils.load_json_from_path(json_report)
    errors = filter(_is_user_visible, errors)
    utils.stdout('\n' + _text_of_report_list(errors))
    plain_out = _text_of_report_list(errors,
                                     formatter=colorize.PLAIN_FORMATTER)
    with codecs.open(bugs_out, 'w',
                     encoding=config.LOCALE, errors='replace') as file_out:
        file_out.write(plain_out)
    if xml_out is not None:
        with codecs.open(xml_out, 'w',
                         encoding=config.LOCALE, errors='replace') as file_out:
            file_out.write(_pmd_xml_of_issues(errors))


def merge_reports_from_paths(report_paths):
    json_data = []
    for json_path in report_paths:
        json_data.extend(utils.load_json_from_path(json_path))
    return _sort_and_uniq_rows(json_data)


def _pmd_xml_of_issues(issues):
    if etree is None:
        print('ERROR: "etree" Python package not found.')
        print('ERROR: You need to install it to use Infer with --pmd-xml')
        sys.exit(1)
    root = etree.Element('pmd')
    root.attrib['version'] = '5.4.1'
    root.attrib['date'] = datetime.datetime.now().isoformat()
    for issue in issues:
        fully_qualifed_method_name = re.search('(.*)\(.*',
                                               issue[JSON_INDEX_PROCEDURE_ID])
        class_name = ''
        package = ''
        if fully_qualifed_method_name is not None:
            # probably Java
            info = fully_qualifed_method_name.groups()[0].split('.')
            class_name = info[-2:-1][0]
            method = info[-1]
            package = '.'.join(info[0:-2])
        else:
            method = issue[JSON_INDEX_PROCEDURE]
        file_node = etree.Element('file')
        file_node.attrib['name'] = issue[JSON_INDEX_FILENAME]
        violation = etree.Element('violation')
        violation.attrib['begincolumn'] = '0'
        violation.attrib['beginline'] = str(issue[JSON_INDEX_LINE])
        violation.attrib['endcolumn'] = '0'
        violation.attrib['endline'] = str(issue[JSON_INDEX_LINE] + 1)
        violation.attrib['class'] = class_name
        violation.attrib['method'] = method
        violation.attrib['package'] = package
        violation.attrib['priority'] = '1'
        violation.attrib['rule'] = issue[JSON_INDEX_TYPE]
        violation.attrib['ruleset'] = 'Infer Rules'
        violation.attrib['externalinfourl'] = (
            ISSUE_TYPES_URL + issue[JSON_INDEX_TYPE])
        violation.text = issue[JSON_INDEX_QUALIFIER]
        file_node.append(violation)
        root.append(file_node)
    return etree.tostring(root, pretty_print=True, encoding=config.LOCALE)


def _sort_and_uniq_rows(l):
    key = operator.itemgetter(JSON_INDEX_FILENAME,
                              JSON_INDEX_LINE,
                              JSON_INDEX_HASH,
                              JSON_INDEX_QUALIFIER)
    l.sort(key=key)
    groups = itertools.groupby(l, key)
    # guaranteed to be at least one element in each group
    return map(lambda (keys, dups): dups.next(), groups)


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
    error_kind = row[CSV_INDEX_KIND]
    error_type = row[CSV_INDEX_TYPE]
    error_bucket = ''  # can be updated later once we extract it from qualifier

    try:
        qualifier_xml = ET.fromstring(row[CSV_INDEX_QUALIFIER_TAGS])
        if qualifier_xml.tag == QUALIFIER_TAGS:
            bucket = qualifier_xml.find(BUCKET_TAGS)
            if bucket is not None:
                error_bucket = bucket.text
    except ET.ParseError:
        pass  # this will skip any invalid xmls

    return _should_report(analyzer, error_kind, error_type, error_bucket)


def _should_report_json(analyzer, row):
    error_kind = row[JSON_INDEX_KIND]
    error_type = row[JSON_INDEX_TYPE]
    error_bucket = ''  # can be updated later once we extract it from qualifier

    for qual_tag in row[QUALIFIER_TAGS]:
        if qual_tag['tag'] == BUCKET_TAGS:
            error_bucket = qual_tag['value']
            break

    return _should_report(analyzer, error_kind, error_type, error_bucket)


def _print_and_write(file_out, message):
    utils.stdout(message)
    file_out.write(utils.encode(message + '\n'))
