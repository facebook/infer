import os
import subprocess
import traceback

import util

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

create_argparser = \
    util.clang_frontend_argparser(MODULE_DESCRIPTION, MODULE_NAME)


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
        wrappers_path = os.path.join(
            os.path.dirname(
                os.path.realpath(__file__)), os.path.pardir, 'wrappers')
        env_vars['INFER_OLD_PATH'] = env_vars['PATH']
        env_vars['PATH'] = '{wrappers}{sep}{path}'.format(
            wrappers=wrappers_path,
            sep=os.pathsep,
            path=env_vars['PATH'],
        )

        frontend_env_vars = util.get_clang_frontend_envvars(self.args)
        env_vars.update(frontend_env_vars)
        return env_vars

    def capture(self):
        self.create_results_dir()

        try:
            subprocess.check_call(self.cmd, env=self.get_envvars())
            return os.EX_OK
        except subprocess.CalledProcessError as exc:
            if self.args.debug:
                traceback.print_exc()
            return exc.returncode
