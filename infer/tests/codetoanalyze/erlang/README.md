# Conventions for Erlang tests

## Directories

Put the test in the folder based on the analyzer that is being tested:
- Pulse: `pulse` or `pulse-otp` if OTP specs (`--erlang-with-otp-specs`) are needed.
- Topl: `topl`.
- Create new files/directories as needed.
- The `compiler` dir is special, see later.

## Naming

Test functions should be named `(fp_|fn_)?test_.*_(Ok|Latent|Bad)`, where
- `_Ok` means that no issue is expected,
- `_Bad` means that an issue is expected,
- `_Latent` means that a latent issue is expected (e.g. because the function has arguments),
- `fp_` can be added to `_Ok` tests if currently Infer reports an issue,
- `fn_` can be added to `_Bad` or `_Latent` tests if currently Infer does not report an issue.
- Furthermore, test functions should be exported.

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
