#!/usr/bin/env python2.7

# Copyright (c) 2013 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import argparse
import csv
import io
import json
import logging
import multiprocessing
import os
import platform
import re
import shutil
import stat
import subprocess
import sys
import tempfile
import time
import traceback
import zipfile

from inferlib import analyze, config, issues, utils


ANALYSIS_SUMMARY_OUTPUT = 'analysis_summary.txt'

DEFAULT_BUCK_OUT = os.path.join(os.getcwd(), 'buck-out')
DEFAULT_BUCK_OUT_GEN = os.path.join(DEFAULT_BUCK_OUT, 'gen')

INFER_CSV_REPORT = os.path.join(config.BUCK_INFER_OUT,
                                config.CSV_REPORT_FILENAME)
INFER_JSON_REPORT = os.path.join(config.BUCK_INFER_OUT,
                                 config.JSON_REPORT_FILENAME)
INFER_STATS = os.path.join(config.BUCK_INFER_OUT, config.STATS_FILENAME)

INFER_SCRIPT = """\
#!/usr/bin/env {0}
import subprocess
import sys

cmd = ['{0}'] + {1} + ['--', 'javac'] + sys.argv[1:]
subprocess.check_call(cmd)
"""

LOCAL_CONFIG = """\
    [tools]
        javac = %s
"""


def prepare_build(args):
    """Creates script that redirects javac calls to infer and a local buck
    configuration that tells buck to use that script.
    """

    infer_options = [
        '--buck',
        '--analyzer', args.analyzer,
    ]

    if args.debug:
        infer_options.append('--debug')

    if args.no_filtering:
        infer_options.append('--no-filtering')

    if args.debug_exceptions:
        infer_options += ['--debug-exceptions', '--no-filtering']

    # Create a temporary directory as a cache for jar files.
    infer_cache_dir = os.path.join(args.infer_out, 'cache')
    if not os.path.isdir(infer_cache_dir):
       os.mkdir(infer_cache_dir)
    infer_options.append('--infer_cache')
    infer_options.append(infer_cache_dir)
    temp_files = [infer_cache_dir]

    try:
        infer = [utils.get_cmd_in_bin_dir('infer')] + infer_options
    except subprocess.CalledProcessError as e:
        logging.error('Could not find infer')
        raise e

    # Disable the use of buckd as this scripts modifies .buckconfig.local
    logging.info('Disabling buckd: export NO_BUCKD=1')
    os.environ['NO_BUCKD'] = '1'

    # make sure INFER_ANALYSIS is set when buck is called
    logging.info('Setup Infer analysis mode for Buck: export INFER_ANALYSIS=1')
    os.environ['INFER_ANALYSIS'] = '1'

    # Create a script to be called by buck
    infer_script = None
    with tempfile.NamedTemporaryFile(delete=False,
                                     prefix='infer_',
                                     suffix='.py',
                                     dir='.') as infer_script:
        logging.info('Creating %s' % infer_script.name)
        infer_script.file.write(
            INFER_SCRIPT.format(sys.executable, infer).encode())

    st = os.stat(infer_script.name)
    os.chmod(infer_script.name, st.st_mode | stat.S_IEXEC)

    temp_files += [infer_script.name]
    return temp_files, infer_script.name


def get_normalized_targets(targets):
    """ Use buck to convert a list of input targets/aliases
        into a set of the (transitive) target deps for all inputs"""

    # this expands the targets passed on the command line, then filters away
    # targets that are not Java/Android. you need to change this if you
    # care about something other than Java/Android
    TARGET_TYPES = "kind('android_library|java_library', deps('%s'))"
    BUCK_GET_JAVA_TARGETS = ['buck', 'query', TARGET_TYPES]
    buck_cmd = BUCK_GET_JAVA_TARGETS + targets

    try:
        targets = filter(
            lambda line: len(line) > 0,
            subprocess.check_output(buck_cmd).decode().strip().split('\n'))
        return targets
    except subprocess.CalledProcessError as e:
        logging.error('Error while expanding targets with {0}'.format(buck_cmd))
        raise e


def init_stats(args, start_time):
    """Returns dictionary with target independent statistics.
    """
    return {
        'float': {},
        'int': {
            'cores': multiprocessing.cpu_count(),
            'time': int(time.time()),
            'start_time': int(round(start_time)),
        },
        'normal': {
            'debug': str(args.debug),
            'analyzer': args.analyzer,
            'machine': platform.machine(),
            'node': platform.node(),
            'project': os.path.basename(os.getcwd()),
            'revision': utils.vcs_revision(),
            'branch': utils.vcs_branch(),
            'system': platform.system(),
            'infer_version': utils.infer_version(),
            'infer_branch': utils.infer_branch(),
        }
    }


def store_performances_csv(infer_out, stats):
    """Stores the statistics about perfromances into a CSV file to be exported
        to a database"""
    perf_filename = os.path.join(infer_out, config.CSV_PERF_FILENAME)
    with open(perf_filename, 'w') as csv_file_out:
        csv_writer = csv.writer(csv_file_out)
        keys = ['infer_version', 'project', 'revision', 'files', 'lines',
                'cores', 'system', 'machine', 'node', 'total_time',
                'capture_time', 'analysis_time', 'reporting_time', 'time']
        int_stats = list(stats['int'].items())
        normal_stats = list(stats['normal'].items())
        flat_stats = dict(int_stats + normal_stats)
        values = []
        for key in keys:
            values.append(flat_stats[key])
        csv_writer.writerow(keys)
        csv_writer.writerow(values)
        csv_file_out.flush()


def get_harness_code():
    all_harness_code = '\nGenerated harness code:\n'
    for filename in os.listdir(DEFAULT_BUCK_OUT_GEN):
        if 'InferGeneratedHarness' in filename:
            all_harness_code += '\n' + filename + ':\n'
            with open(os.path.join(DEFAULT_BUCK_OUT_GEN,
                                   filename), 'r') as file_in:
                all_harness_code += file_in.read()
    return all_harness_code + '\n'


def get_basic_stats(stats):
    files_analyzed = '{0} files ({1} lines) analyzed in {2}s\n\n'.format(
        stats['int']['files'],
        stats['int']['lines'],
        stats['int']['total_time'],
    )
    phase_times = 'Capture time: {0}s\nAnalysis time: {1}s\n\n'.format(
        stats['int']['capture_time'],
        stats['int']['analysis_time'],
    )

    to_skip = {
        'files',
        'procedures',
        'lines',
        'cores',
        'time',
        'start_time',
        'capture_time',
        'analysis_time',
        'reporting_time',
        'total_time',
        'makefile_generation_time'
    }
    bugs_found = 'Errors found:\n\n'
    for key, value in sorted(stats['int'].items()):
        if key not in to_skip:
            bugs_found += '  {0:>8}  {1}\n'.format(value, key)

    basic_stats_message = files_analyzed + phase_times + bugs_found + '\n'
    return basic_stats_message


def get_buck_stats():
    trace_filename = os.path.join(
        DEFAULT_BUCK_OUT,
        'log',
        'traces',
        'build.trace'
    )
    ARGS = 'args'
    SUCCESS_STATUS = 'success_type'
    buck_stats = {}
    try:
        trace = utils.load_json_from_path(trace_filename)
        for t in trace:
            if SUCCESS_STATUS in t[ARGS]:
                status = t[ARGS][SUCCESS_STATUS]
                count = buck_stats.get(status, 0)
                buck_stats[status] = count + 1

        buck_stats_message = 'Buck build statistics:\n\n'
        for key, value in sorted(buck_stats.items()):
            buck_stats_message += '  {0:>8}  {1}\n'.format(value, key)

        return buck_stats_message
    except IOError as e:
        logging.error('Caught %s: %s' % (e.__class__.__name__, str(e)))
        logging.error(traceback.format_exc())
        return ''


class NotFoundInJar(Exception):
    pass


def load_stats(opened_jar):
    try:
        return json.loads(opened_jar.read(INFER_STATS).decode())
    except KeyError as e:
        raise NotFoundInJar


def load_csv_report(opened_jar):
    try:
        sio = io.StringIO(opened_jar.read(INFER_CSV_REPORT).decode())
        return list(utils.locale_csv_reader(sio))
    except KeyError as e:
        raise NotFoundInJar


def load_json_report(opened_jar):
    try:
        return json.loads(opened_jar.read(INFER_JSON_REPORT).decode())
    except KeyError as e:
        raise NotFoundInJar


def collect_results(args, start_time):
    """Walks through buck-gen, collects results for the different buck targets
    and stores them in in args.infer_out/results.csv.
    """
    buck_stats = get_buck_stats()
    logging.info(buck_stats)
    with open(os.path.join(args.infer_out, ANALYSIS_SUMMARY_OUTPUT), 'w') as f:
        f.write(buck_stats)

    all_csv_rows = set()
    all_json_rows = set()
    headers = []
    stats = init_stats(args, start_time)

    accumulation_whitelist = list(map(re.compile, [
        '^cores$',
        '^time$',
        '^start_time$',
        '.*_pc',
    ]))

    expected_analyzer = stats['normal']['analyzer']
    expected_version = stats['normal']['infer_version']

    for root, _, files in os.walk(DEFAULT_BUCK_OUT_GEN):
        for f in [f for f in files if f.endswith('.jar')]:
            path = os.path.join(root, f)
            try:
                with zipfile.ZipFile(path) as jar:
                    # Accumulate integers and float values
                    target_stats = load_stats(jar)

                    found_analyzer = target_stats['normal']['analyzer']
                    found_version = target_stats['normal']['infer_version']

                    if (found_analyzer != expected_analyzer
                            or found_version != expected_version):
                        continue
                    else:
                        for type_k in ['int', 'float']:
                            items = target_stats.get(type_k, {}).items()
                            for key, value in items:
                                if not any(map(lambda r: r.match(key),
                                           accumulation_whitelist)):
                                    old_value = stats[type_k].get(key, 0)
                                    stats[type_k][key] = old_value + value

                    csv_rows = load_csv_report(jar)
                    if len(csv_rows) > 0:
                        headers.append(csv_rows[0])
                        for row in csv_rows[1:]:
                            all_csv_rows.add(tuple(row))

                    json_rows = load_json_report(jar)
                    for row in json_rows:
                        all_json_rows.add(json.dumps(row))

                    # Override normals
                    stats['normal'].update(target_stats.get('normal', {}))
            except NotFoundInJar:
                pass
            except zipfile.BadZipfile:
                logging.warn('Bad zip file %s', path)

    csv_report = os.path.join(args.infer_out, config.CSV_REPORT_FILENAME)
    json_report = os.path.join(args.infer_out, config.JSON_REPORT_FILENAME)
    bugs_out = os.path.join(args.infer_out, config.BUGS_FILENAME)

    if len(headers) == 0:
        with open(csv_report, 'w'):
            pass
        logging.info('No reports found')
        return
    elif len(headers) > 1:
        if any(map(lambda x: x != headers[0], headers)):
            raise Exception('Inconsistent reports found')

    # Convert all float values to integer values
    for key, value in stats.get('float', {}).items():
        stats['int'][key] = int(round(value))

    # Delete the float entries before exporting the results
    del(stats['float'])

    with open(csv_report, 'w') as report:
        writer = csv.writer(report)
        all_csv_rows = [list(row) for row in all_csv_rows]
        writer.writerows([headers[0]] + all_csv_rows)
        report.flush()

    with open(json_report, 'w') as report:
        json_string = '['
        json_string += ','.join(all_json_rows)
        json_string += ']'
        report.write(json_string)
        report.flush()

    print('\n')
    xml_out = None
    if args.pmd_xml is not None:
        xml_out = os.path.join(args.infer_out,
                               config.PMD_XML_FILENAME)
    issues.print_and_save_errors(json_report, bugs_out, xml_out)

    stats['int']['total_time'] = int(round(utils.elapsed_time(start_time)))

    store_performances_csv(args.infer_out, stats)

    stats_filename = os.path.join(args.infer_out, config.STATS_FILENAME)
    utils.dump_json_to_path(stats, stats_filename)

    basic_stats = get_basic_stats(stats)

    if args.print_harness:
        harness_code = get_harness_code()
        basic_stats += harness_code

    logging.info(basic_stats)

    with open(os.path.join(args.infer_out, ANALYSIS_SUMMARY_OUTPUT), 'a') as f:
        f.write(basic_stats)


def cleanup(temp_files):
    """Removes the generated .buckconfig.local and the temporary infer script.
    """
    for file in temp_files:
        try:
            logging.info('Removing %s' % file)
            if os.path.isdir(file):
              shutil.rmtree(file)
            else:
              os.unlink(file)
        except IOError:
            logging.error('Could not remove %s' % file)


parser = argparse.ArgumentParser()
parser.add_argument('--build-report', metavar='PATH', type=str)
parser.add_argument('--deep', action='store_true')
parser.add_argument('--keep-going', action='store_true')
parser.add_argument('--load-limit', '-L')
parser.add_argument('--no-cache', action='store_true')
parser.add_argument('--profile', action='store_true')
parser.add_argument('--shallow', action='store_true')
parser.add_argument('--num-threads', '-j', metavar='N')
parser.add_argument('--verbose', '-v', metavar='N', type=int)
parser.add_argument('targets', nargs='*', metavar='target',
                    help='Build targets to analyze')


class UnsuportedBuckCommand(Exception):
    pass


def parse_buck_command(args):
    build_keyword = 'build'
    if build_keyword in args and len(args[args.index(build_keyword):]) > 1:
        next_index = args.index(build_keyword) + 1
        buck_args = args[next_index:]
        parsed_args = parser.parse_args(buck_args)
        base_cmd_without_targets = [p for p in buck_args
                                    if p not in parsed_args.targets]
        base_cmd = ['buck', build_keyword] + base_cmd_without_targets
        return base_cmd, parsed_args

    else:
        raise UnsuportedBuckCommand(args)


class Wrapper:

    def __init__(self, infer_args, buck_cmd):
        self.timer = utils.Timer(logging.info)
        self.infer_args = infer_args
        self.timer.start('Computing library targets')
        base_cmd, buck_args = parse_buck_command(buck_cmd)
        self.normalized_targets = get_normalized_targets(
            buck_args.targets)
        self.buck_cmd = base_cmd + self.normalized_targets
        self.timer.stop('%d targets computed', len(self.normalized_targets))

    def run(self):
        temp_files = []
        try:
            start_time = time.time()
            logging.info('Starting the analysis')

            if not os.path.isdir(self.infer_args.infer_out):
                os.mkdir(self.infer_args.infer_out)

            self.timer.start('Preparing build...')
            temp_files2, infer_script = prepare_build(self.infer_args)
            temp_files += temp_files2
            self.timer.stop('Build prepared')

            if len(self.normalized_targets) == 0:
                logging.info('Nothing to analyze')
            else:
                self.timer.start('Running buck...')
                javac_config = ['--config', 'tools.javac=' + infer_script]
                buck_cmd = self.buck_cmd + javac_config
                subprocess.check_call(buck_cmd)
                self.timer.stop('Buck finished')
                self.timer.start('Collecting results...')
                collect_results(self.infer_args, start_time)
                self.timer.stop('Done')
            return os.EX_OK
        except KeyboardInterrupt as e:
            self.timer.stop('Exiting')
            sys.exit(0)
        finally:
            cleanup(temp_files)
