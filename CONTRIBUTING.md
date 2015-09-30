
# Contribution Guidelines

## Contributor License Agreement
We require contributors to sign our Contributor License Agreement. In order for us to review and merge your code, please sign up at https://code.facebook.com/cla. If you have any questions, please drop us a line at cla@fb.com. Thanks!

## Submitting Pull-Requests
* Please make sure Infer builds : `make -C infer clean && make -C infer <target>`, refer to the installation document for details
* If relevant, add a test for your change. Tests are located at `infer/tests/codetoanalyze/` and `infer/tests/{endtoend,frontend}/`. To run the all the tests, execute: `./scripts/test.sh`
