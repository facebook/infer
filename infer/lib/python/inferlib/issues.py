# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import codecs
import datetime
import itertools
import operator
import os
import re
import sys

try:
    from lxml import etree
except ImportError:
    etree = None

from . import colorize, config, source, utils


ISSUE_SEVERITY_ERROR = 'ERROR'
ISSUE_SEVERITY_WARNING = 'WARNING'
ISSUE_SEVERITY_INFO = 'INFO'
ISSUE_SEVERITY_ADVICE = 'ADVICE'
ISSUE_SEVERITY_LIKE = 'LIKE'

# field names in rows of json reports
JSON_INDEX_CENSORED_REASON = 'censored_reason'
JSON_INDEX_DOTTY = 'dotty'
JSON_INDEX_FILENAME = 'file'
JSON_INDEX_HASH = 'hash'
JSON_INDEX_INFER_SOURCE_LOC = 'infer_source_loc'
JSON_INDEX_ISL_FILE = 'file'
JSON_INDEX_ISL_LNUM = 'lnum'
JSON_INDEX_ISL_CNUM = 'cnum'
JSON_INDEX_ISL_ENUM = 'enum'
JSON_INDEX_SEVERITY = 'severity'
JSON_INDEX_LINE = 'line'
JSON_INDEX_PROCEDURE = 'procedure'
JSON_INDEX_QUALIFIER = 'qualifier'
JSON_INDEX_QUALIFIER_TAGS = 'qualifier_tags'
JSON_INDEX_TYPE = 'bug_type'
JSON_INDEX_TRACE = 'bug_trace'
JSON_INDEX_TRACE_LEVEL = 'level'
JSON_INDEX_TRACE_FILENAME = 'filename'
JSON_INDEX_TRACE_LINE = 'line_number'
JSON_INDEX_TRACE_COLUMN = 'column_number'
JSON_INDEX_TRACE_DESCRIPTION = 'description'
JSON_INDEX_TRACEVIEW_ID = 'traceview_id'


ISSUE_TYPES_URL = 'http://fbinfer.com/docs/infer-issue-types.html#'


def text_of_infer_loc(loc):
    return ' ({}:{}:{}-{}:)'.format(
        loc[JSON_INDEX_ISL_FILE],
        loc[JSON_INDEX_ISL_LNUM],
        loc[JSON_INDEX_ISL_CNUM],
        loc[JSON_INDEX_ISL_ENUM],
    )


def text_of_report(report):
    filename = report[JSON_INDEX_FILENAME]
    severity = report[JSON_INDEX_SEVERITY]
    line = report[JSON_INDEX_LINE]
    error_type = report[JSON_INDEX_TYPE]
    msg = report[JSON_INDEX_QUALIFIER]
    infer_loc = ''
    if JSON_INDEX_INFER_SOURCE_LOC in report:
        infer_loc = text_of_infer_loc(report[JSON_INDEX_INFER_SOURCE_LOC])
    return '%s:%d: %s: %s%s\n  %s' % (
        filename,
        line,
        severity.lower(),
        error_type,
        infer_loc,
        msg,
    )


def _text_of_report_list(project_root, reports, bugs_txt_path, limit=None,
                         console_out=False,
                         formatter=colorize.TERMINAL_FORMATTER):
    n_issues = len(reports)
    if n_issues == 0:
        msg = 'No issues found'
        if formatter == colorize.TERMINAL_FORMATTER:
            msg = colorize.color('  %s  ' % msg,
                                 colorize.SUCCESS, formatter)
        if console_out:
            utils.stderr(msg)
        return msg

    text_errors_list = []
    for report in reports[:limit]:
        filename = report[JSON_INDEX_FILENAME]
        line = report[JSON_INDEX_LINE]

        source_context = ''
        source_context = source.build_source_context(
            os.path.join(project_root, filename),
            formatter,
            line,
            1,
            True
        )
        indenter = source.Indenter() \
                         .indent_push() \
                         .add(source_context)
        source_context = '\n' + unicode(indenter)

        msg = text_of_report(report)
        if report[JSON_INDEX_SEVERITY] == ISSUE_SEVERITY_ERROR:
            msg = colorize.color(msg, colorize.ERROR, formatter)
        elif report[JSON_INDEX_SEVERITY] == ISSUE_SEVERITY_WARNING:
            msg = colorize.color(msg, colorize.WARNING, formatter)
        elif report[JSON_INDEX_SEVERITY] == ISSUE_SEVERITY_ADVICE:
            msg = colorize.color(msg, colorize.ADVICE, formatter)
        elif report[JSON_INDEX_SEVERITY] == ISSUE_SEVERITY_LIKE:
            msg = colorize.color(msg, colorize.LIKE, formatter)
        text = '%s%s' % (msg, source_context)
        text_errors_list.append(text)

    error_types_count = {}
    for report in reports:
        t = report[JSON_INDEX_TYPE]
        # assert failures are not very informative without knowing
        # which assertion failed
        if t == 'Assert_failure' and JSON_INDEX_INFER_SOURCE_LOC in report:
            t += text_of_infer_loc(report[JSON_INDEX_INFER_SOURCE_LOC])
        if t not in error_types_count:
            error_types_count[t] = 1
        else:
            error_types_count[t] += 1

    max_type_length = max(map(len, error_types_count.keys())) + 2
    sorted_error_types = error_types_count.items()
    sorted_error_types.sort(key=operator.itemgetter(1), reverse=True)
    types_text_list = map(lambda (t, count): '%s: %d' % (
        t.rjust(max_type_length),
        count,
    ), sorted_error_types)

    text_errors = '\n\n'.join(text_errors_list)
    if limit >= 0 and n_issues > limit:
        text_errors += colorize.color(
            ('\n\n...too many issues to display (limit=%d exceeded), please ' +
             'see %s or run `infer-explore` for the remaining issues.')
            % (limit, bugs_txt_path), colorize.HEADER, formatter)

    issues_found = 'Found {n_issues}'.format(
        n_issues=utils.get_plural('issue', n_issues),
    )
    bug_list = '{issues_found}\n\n{issues}\n\n'.format(
        issues_found=colorize.color(issues_found,
                                    colorize.HEADER,
                                    formatter),
        issues=text_errors,
    )
    summary = '{header}\n\n{summary}'.format(
        header=colorize.color('Summary of the reports',
                              colorize.HEADER, formatter),
        summary='\n'.join(types_text_list),
    )

    if console_out:
        utils.stderr(bug_list)
        utils.stdout(summary)

    return bug_list + summary
