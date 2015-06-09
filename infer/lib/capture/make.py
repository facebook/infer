import argparse
import os
import subprocess
import traceback

MODULE_NAME = 'make/cc/clang/gcc'
MODULE_DESCRIPTION = '''Run analysis of code built with commands like:
make [target]
clang [compiler_options] <filename>
gcc [compiler_options] <filename>
cc [compiler_options] <filename>

Analysis examples:
infer -- make all
infer -- clang -c srcfile.m
infer -- gcc -c srcfile.c'''


def gen_instance(*args):
    return MakeCapture(*args)


def mkdir_if_not_exists(path):
    if not os.path.exists(path):
        os.mkdir(path)


def create_argparser(group_name=MODULE_NAME):
    """This defines the set of arguments that get added by this module to the
    set of global args defined in the infer top-level module
    Do not use this function directly, it should be invoked by the infer
    top-level module"""
    parser = argparse.ArgumentParser(add_help=False)
    group = parser.add_argument_group(
        "{grp} module".format(grp=MODULE_NAME),
        description=MODULE_DESCRIPTION,
    )
    group.add_argument(
        '-hd', '--headers',
        action='store_true',
        help='Analyze code in header files',
    )
    group.add_argument(
        '--models_mode',
        action='store_true',
        dest='models_mode',
        help='Mode for computing the models',
    )
    group.add_argument(
        '--no_failures_allowed',
        action='store_true',
        dest='no_failures_allowed',
        help='Fail if at least one of the translations fails',
    )
    group.add_argument(
        '-tm', '--testing_mode',
        dest='testing_mode',
        action='store_true',
        help='Testing mode for the translation: Do not translate libraries'
             ' (including enums)')
    group.add_argument(
        '-fs', '--frontend-stats',
        dest='frontend_stats',
        action='store_true',
        help='Output statistics about the capture phase to *.o.astlog')
    group.add_argument(
        '-fd', '--frontend-debug',
        dest='frontend_debug',
        action='store_true',
        help='Output debugging information to *.o.astlog during capture')
    return parser


class MakeCapture:
    def __init__(self, args, cmd):
        self.args = args
        self.cmd = [os.path.basename(cmd[0])] + cmd[1:]

    def create_results_dir(self):
        results_dir = self.args.infer_out
        mkdir_if_not_exists(results_dir)
        mkdir_if_not_exists(os.path.join(results_dir, 'specs'))
        mkdir_if_not_exists(os.path.join(results_dir, 'captured'))
        mkdir_if_not_exists(os.path.join(results_dir, 'sources'))

    def get_envvars(self):
        env_vars = dict(os.environ)
        env_vars['INFER_RESULTS_DIR'] = self.args.infer_out
        wrappers_path = os.path.join(
            os.path.dirname(os.path.realpath(__file__)), '..', 'wrappers')
        env_vars['INFER_OLD_PATH'] = env_vars['PATH']
        env_vars['PATH'] = '{wrappers}{sep}{path}'.format(
            wrappers=wrappers_path,
            sep=os.pathsep,
            path=env_vars['PATH'],
        )
        return env_vars

    def capture(self):
        self.create_results_dir()

        env_vars = self.get_envvars()
        frontend_args = []

        if self.args.headers:
            frontend_args.append('-headers')
        if self.args.models_mode:
            frontend_args.append('-models_mode')
        if self.args.project_root:
            frontend_args += ['-project_root', self.args.project_root]
        if self.args.testing_mode:
            frontend_args.append('-testing_mode')
        if self.args.frontend_debug:
            frontend_args += ['-debug']
            env_vars['FCP_DEBUG_MODE'] = '1'
        if self.args.frontend_stats:
            frontend_args += ['-stats']
            env_vars['FCP_DEBUG_MODE'] = '1'
        if self.args.no_failures_allowed:
            env_vars['FCP_REPORT_FRONTEND_FAILURE'] = '1'

        # export an env variable with all the arguments to pass to InferClang
        env_vars['FCP_INFER_FRONTEND_ARGS'] = ' '.join(frontend_args)

        try:
            subprocess.check_call(self.cmd, env=env_vars)
            return os.EX_OK
        except subprocess.CalledProcessError as exc:
            if self.args.debug:
                traceback.print_exc()
            return exc.returncode
