# Conventions for Erlang tests

## Directories

Put the test in the folder based on the analyzer that is being tested:
- Pulse: `pulse` or `pulse-otp` if OTP specs (`--erlang-with-otp-specs`) are needed.
- Topl: `topl`.
- Create new files/directories as needed.
- The `compiler` dir is special, see later.

## Naming

Test functions should be named `(fp_|fpl_|fn_|fnl_)?test_.*_(Ok|Latent|Bad)`, where the suffix (`_Ok`, `_Bad`, `_Latent`) indicates the expected outcome:
- `_Ok` means that no issue is expected,
- `_Bad` means that a (manifest) issue is expected,
- `_Latent` means that a [latent issue](https://fbinfer.com/docs/next/checker-pulse/#latent-issues) is expected (e.g. because the function has arguments).

The prefixes (`fp_`, `fpl_`, `fn_`, `fnl_`) correspond to deviations with the reported output:
- `fp_` means a false positive: no issue, or a latent issue was expected, but Infer reports a manifest issue.
- `fpl_` is false positive latent, meaning that no issue was expected but a latent one is reported.
- `fn_` means a false negative: manifest or latent issue is expected, but no issue is reported.
- `fnl_` is false negative latent: a manifest issue is expected, but only latent is reported.

For example, for a function `test_f`:

|              |              |                    | **Reported**     |                    |
|--------------|--------------|--------------------|------------------|--------------------|
|              |              | **Ok**             | **Latent**       | **Manifest**       |
|              | **Ok**       | `test_f_Ok`        | `fpl_test_f_Ok`  | `fp_test_f_Ok`     |
| **Expected** | **Latent**   | `fn_test_f_Latent` | `test_f_Latent`  | `fp_test_f_Latent` |
|              | **Manifest** | `fn_test_f_Bad`    | `fnl_test_f_Bad` | `test_f_Bad`       |

Furthermore, test functions should be exported.

## Test structure

Tests usually come in pairs, where they both exercise the same feature (e.g. do the same computation) but one is expected to succeed and one is expected to crash. This is to make sure that Infer reports an issue if and only if expected. Example:

```erlang
test_add_Ok() ->
  X = 2,
  Y = 3,
  case X + Y of
    5 -> ok;
    _ -> ?UNEXPECTED_CRASH
  end.
```

and

```erlang
test_add_Bad() ->
  X = 2,
  Y = 3,
  case X + Y of
    5 -> ?EXPECTED_CRASH;
    _ -> ok
  end.
```

or in shorter form using macros (first argument is expected, second argument is actual):

```erlang
test_add_Ok() -> ?ASSERT_EQUAL(5, 2 + 3).
```

and

```erlang
test_add_Bad() -> ?CRASH_IF_EQUAL(5, 2 + 3).
```

## Other best practices

- If you include the directory of the tests in [compiler/Makefile](compiler/Makefile) all the exported functions - following the naming conventions - with 0 arguments will be compiled and executed. Tests with `_Ok` should terminate successfully, while tests with `_Bad` should crash. This helps in validating that Infer captures the right semantics.
- Use the macros in [common.hrl](./common.hrl) to introduce intentional crashes.
- Even though the tests exercise the whole pipeline (compilation, translation, analysis, reporting, ...), try to make them as isolated as possible (focusing only on a particular feature).
