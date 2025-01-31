# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

async def await_it(arg):
    await arg

async def dont_await_it(arg):
    pass

@final
class C:

    @staticmethod
    async def async_fun():
        pass


def wait(arg):
    """
    This function is expected to be modeled as awaiting its argument because of .inferconfig
    """
    pass

def deep_wait(*arg):
    """
    This function is expected to be modeled as awaiting deeply its arguments because of .inferconfig
    """
    pass
