#!/usr/bin/env python2.7

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

import argparse
import json
import logging
import os
import shutil
import stat
import subprocess
import sys
import tempfile
import time
import zipfile

from inferlib import config, issues, utils

INFER_JSON_REPORT = os.path.join(config.BUCK_INFER_OUT,
                                 config.JSON_REPORT_FILENAME)

INFER_JSON_COSTS_REPORT = os.path.join(config.BUCK_INFER_OUT,
                                       config.JSON_COSTS_REPORT_FILENAME)

INFER_SCRIPT_NAME = 'infer_wrapper.py'
INFER_SCRIPT = """\
#!/usr/bin/env python2.7
import subprocess
import sys

cmd = {infer_command} + ['--', 'javac'] + sys.argv[1:]
subprocess.check_call(cmd)
"""


def prepare_build(args):
    """Creates script that redirects javac calls to infer and a local buck
    configuration that tells buck to use that script.
    """

    infer_options = ['--buck', '--jobs', '1']

    if args.java_jar_compiler is not None:
        infer_options += [
            '--java-jar-compiler',
            args.java_jar_compiler,
        ]

    temp_files = []

    try:
        infer_command = [utils.get_cmd_in_bin_dir('infer')] + infer_options
    except subprocess.CalledProcessError as e:
        logging.error('Could not find infer')
        raise e

    # Create a script to be called by buck
    infer_script_path = os.path.join(os.getcwd(), INFER_SCRIPT_NAME)
    if os.path.exists(infer_script_path):
        raise Exception('{} already exists. Exiting'.format(infer_script_path))
    with open(infer_script_path, 'w') as infer_script:
        logging.info('Creating %s' % infer_script_path)
        infer_script.write(
            utils.encode(INFER_SCRIPT.format(
                infer_command=infer_command)))

    st = os.stat(infer_script_path)
    os.chmod(infer_script_path, st.st_mode | stat.S_IEXEC)

    temp_files += [infer_script_path]
    return temp_files, infer_script_path


def get_normalized_targets(buck_args):
    """ Use buck to convert a list of input targets/aliases
        into a set of the (transitive) target deps for all inputs"""

    if buck_args.deep:
        # this expands the targets passed on the command line, then filters
        # away targets that are not Java/Android. You need to change this if
        # you care about something other than Java/Android
        TARGET_TYPES = "kind('^(android|java)_library$', deps('%s'))"
        BUCK_GET_JAVA_TARGETS = ['buck', 'query', TARGET_TYPES]
        buck_cmd = BUCK_GET_JAVA_TARGETS + buck_args.targets
    else:
        BUCK_RESOLVE_ALIASES = ['buck', 'targets', '--resolve-alias']
        buck_cmd = BUCK_RESOLVE_ALIASES + buck_args.targets
    try:
        targets = filter(
            lambda line: len(line) > 0,
            subprocess.check_output(buck_cmd).decode().strip().split('\n'))
        return targets
    except subprocess.CalledProcessError as e:
        logging.error('Error while resolving targets with {0}'.format(
            buck_cmd))
        raise e


class NotFoundInJar(Exception):
    pass


def load_json_report(opened_jar, path):
    try:
        return json.loads(opened_jar.read(path).decode('utf-8'))
    except KeyError:
        raise NotFoundInJar


def get_output_jars(buck_args, targets):
    if len(targets) == 0:
        return []
    elif buck_args.deep:
        audit_output = subprocess.check_output(
            ['buck', 'audit', 'classpath'] + targets)
        targets_jars = audit_output.strip().split('\n')
    else:
        targets_output = subprocess.check_output(
            ['buck', 'targets', '--show-output'] + targets)
        parsed_output = (
            entry.split()
            for entry in targets_output.decode().strip().split('\n'))
        targets_jars = (p[1] for p in parsed_output if len(p) > 1)
    return filter(os.path.isfile, targets_jars)


def get_key(e):
    return (e[issues.JSON_INDEX_FILENAME], e[issues.JSON_INDEX_TYPE],
            e[issues.JSON_INDEX_LINE], e[issues.JSON_INDEX_QUALIFIER])


# removes duplicate reports when the some file belongs to several Buck targets
def merge_reports(report, collected):
    for e in report:
        key = get_key(e)
        if key not in collected:
            collected[key] = e


def collect_results(buck_args, infer_args, start_time, targets):
    """Walks through buck-out/, collects results for the different buck targets
    and stores them in in args.infer_out/results.json.
    """
    collected_reports = {}
    collected_costs_reports = []

    for path in get_output_jars(buck_args, targets):
        try:
            with zipfile.ZipFile(path) as jar:
                report = load_json_report(jar, INFER_JSON_REPORT)
                costs_report = load_json_report(jar, INFER_JSON_COSTS_REPORT)
                merge_reports(report, collected_reports)
                # No need to de-duplicate elements in costs-report, merge all
                collected_costs_reports += costs_report
        except NotFoundInJar:
            pass
        except zipfile.BadZipfile:
            logging.warn('Bad zip file %s', path)

    json_report = os.path.join(infer_args.infer_out,
                               config.JSON_REPORT_FILENAME)
    json_costs_report = os.path.join(infer_args.infer_out,
                                     config.JSON_COSTS_REPORT_FILENAME)

    with open(json_report, 'w') as report_out, \
            open(json_costs_report, 'w') as costs_report_out:
        json.dump(collected_reports.values(), report_out)
        json.dump(collected_costs_reports, costs_report_out)

    bugs_out = os.path.join(infer_args.infer_out, config.BUGS_FILENAME)
    issues.print_and_save_errors(infer_args.infer_out, infer_args.project_root,
                                 json_report, bugs_out, infer_args.pmd_xml,
                                 console_out=not infer_args.quiet)


def cleanup(temp_files):
    """Removes the temporary infer files.
    """
    for file in temp_files:
        try:
            logging.info('Removing %s' % file)
            if os.path.isdir(file):
                shutil.rmtree(file)
            else:
                os.unlink(file)
        except OSError:
            logging.error('Could not remove %s' % file)


parser = argparse.ArgumentParser()
parser.add_argument('--build-report', metavar='PATH', type=utils.decode)
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
        buck_build_command = ['buck', build_keyword]
        base_cmd = buck_build_command + base_cmd_without_targets
        return base_cmd, parsed_args

    else:
        raise UnsuportedBuckCommand(args)


class Wrapper:

    def __init__(self, infer_args, buck_cmd):
        self.timer = utils.Timer(logging.info)
        self.infer_args = infer_args
        self.timer.start('Computing library targets')
        base_cmd, buck_args = parse_buck_command(buck_cmd)
        self.buck_args = buck_args
        self.normalized_targets = get_normalized_targets(buck_args)
        self.temp_files = []
        # write targets to file to avoid passing too many command line args
        with tempfile.NamedTemporaryFile(delete=False,
                                         prefix='targets_') as targets_file:
            targets_file.write('\n'.join(self.normalized_targets))
            self.buck_cmd = base_cmd + ['@%s' % targets_file.name]
            self.temp_files.append(targets_file.name)
        self.timer.stop('%d targets computed', len(self.normalized_targets))

    def _collect_results(self, start_time):
        self.timer.start('Collecting results ...')
        collect_results(self.buck_args, self.infer_args, start_time,
                        self.normalized_targets)
        self.timer.stop('Done')

    def run(self):
        start_time = time.time()
        try:
            logging.info('Starting the analysis')

            if not os.path.isdir(self.infer_args.infer_out):
                os.mkdir(self.infer_args.infer_out)

            self.timer.start('Preparing build ...')
            temp_files2, infer_script = prepare_build(self.infer_args)
            self.temp_files += temp_files2
            self.timer.stop('Build prepared')

            if len(self.normalized_targets) == 0:
                logging.info('Nothing to analyze')
            else:
                self.timer.start('Running Buck ...')
                buck_config = [
                    '--config', 'client.id=infer.java',
                    '--config', 'java.abi_generation_mode=class',
                    '--config', 'infer.no_custom_javac=true',
                    '--config', 'tools.javac=' + infer_script,
                    # make sure the javac_jar option is disabled to
                    # avoid conficts with the javac option
                    '--config', 'tools.javac_jar=',
                ]
                buck_cmd = self.buck_cmd + buck_config
                subprocess.check_call(buck_cmd)
                self.timer.stop('Buck finished')
            self._collect_results(start_time)
            return os.EX_OK
        except KeyboardInterrupt as e:
            self.timer.stop('Exiting')
            sys.exit(0)
        except subprocess.CalledProcessError as e:
            if self.buck_args.keep_going:
                print('Buck failed, but continuing the analysis '
                      'because --keep-going was passed')
                self._collect_results(start_time)
                return os.EX_OK
            raise e
        finally:
            cleanup(self.temp_files)
