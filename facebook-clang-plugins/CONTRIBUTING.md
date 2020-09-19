# Contributing to facebook-clang-plugins
We want to make contributing to this project as easy and transparent as
possible.

## Code of Conduct
The code of conduct is described in [`CODE_OF_CONDUCT.md`](CODE_OF_CONDUCT.md).

## Our Development Process
The github repository https://github.com/facebook/facebook-clang-plugins is the source of truth.

## Pull Requests
We actively welcome your pull requests.

1. Fork the repo and create your branch from `master`.
2. If you've added code that should be tested, add tests
3. If you've changed APIs, update the documentation.
4. Ensure the test suite passes. 
  Run tests with `make -C libtooling test`. For re-recording the tests, run `make -C libtooling record-test-outputs`.
5. If you haven't already, complete the Contributor License Agreement ("CLA").

## Contributor License Agreement ("CLA")
In order to accept your pull request, we need you to submit a CLA. You only need
to do this once to work on any of Facebook's open source projects.

Complete your CLA here: <https://developers.facebook.com/opensource/cla>

## Issues
We use GitHub issues to track public bugs. Please ensure your description is
clear and has sufficient instructions to be able to reproduce the issue.

Facebook has a [bounty program](https://www.facebook.com/whitehat/) for the safe
disclosure of security bugs. In those cases, please go through the process
outlined on that page and do not file a public issue.

## Coding Style
Please use the LLVM style for new code: http://llvm.org/docs/CodingStandards.html
while we are (slowly) updating the rest of the files.

## License
By contributing to facebook-clang-plugins, you agree that your contributions will be licensed
under its MIT license.
