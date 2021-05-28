---
title: "Quandary"
description: "The Quandary taint analysis detects flows of values between sources and sinks, except if the value went through a \"sanitizer\". In addition to some defaults, users can specify their own sources, sinks, and sanitizers functions."
---

The Quandary taint analysis detects flows of values between sources and sinks, except if the value went through a "sanitizer". In addition to some defaults, users can specify their own sources, sinks, and sanitizers functions.

Activate with `--quandary`.

Supported languages:
- C/C++/ObjC: Yes
- C#/.Net: Yes
- Erlang: Yes
- Java: Yes

Quandary is a static taint analyzer that identifies a variety of unsafe
information flows. It has a small list of built-in
[sources](https://github.com/facebook/infer/blob/master/infer/src/quandary/JavaTrace.ml#L36)
and
[sinks](https://github.com/facebook/infer/blob/master/infer/src/quandary/JavaTrace.ml#L178),
and you can define custom sources and sinks in your `.inferconfig` file (see
example
[here](https://github.com/facebook/infer/blob/master/infer/tests/codetoanalyze/java/quandary/.inferconfig)).


## List of Issue Types

The following issue types are reported by this checker:
- [CREATE_INTENT_FROM_URI](/docs/next/all-issue-types#create_intent_from_uri)
- [CROSS_SITE_SCRIPTING](/docs/next/all-issue-types#cross_site_scripting)
- [EXPOSED_INSECURE_INTENT_HANDLING](/docs/next/all-issue-types#exposed_insecure_intent_handling)
- [INSECURE_INTENT_HANDLING](/docs/next/all-issue-types#insecure_intent_handling)
- [JAVASCRIPT_INJECTION](/docs/next/all-issue-types#javascript_injection)
- [LOGGING_PRIVATE_DATA](/docs/next/all-issue-types#logging_private_data)
- [QUANDARY_TAINT_ERROR](/docs/next/all-issue-types#quandary_taint_error)
- [SHELL_INJECTION](/docs/next/all-issue-types#shell_injection)
- [SHELL_INJECTION_RISK](/docs/next/all-issue-types#shell_injection_risk)
- [SQL_INJECTION](/docs/next/all-issue-types#sql_injection)
- [SQL_INJECTION_RISK](/docs/next/all-issue-types#sql_injection_risk)
- [UNTRUSTED_BUFFER_ACCESS](/docs/next/all-issue-types#untrusted_buffer_access)
- [UNTRUSTED_DESERIALIZATION](/docs/next/all-issue-types#untrusted_deserialization)
- [UNTRUSTED_DESERIALIZATION_RISK](/docs/next/all-issue-types#untrusted_deserialization_risk)
- [UNTRUSTED_ENVIRONMENT_CHANGE_RISK](/docs/next/all-issue-types#untrusted_environment_change_risk)
- [UNTRUSTED_FILE](/docs/next/all-issue-types#untrusted_file)
- [UNTRUSTED_FILE_RISK](/docs/next/all-issue-types#untrusted_file_risk)
- [UNTRUSTED_HEAP_ALLOCATION](/docs/next/all-issue-types#untrusted_heap_allocation)
- [UNTRUSTED_INTENT_CREATION](/docs/next/all-issue-types#untrusted_intent_creation)
- [UNTRUSTED_URL_RISK](/docs/next/all-issue-types#untrusted_url_risk)
- [UNTRUSTED_VARIABLE_LENGTH_ARRAY](/docs/next/all-issue-types#untrusted_variable_length_array)
- [USER_CONTROLLED_SQL_RISK](/docs/next/all-issue-types#user_controlled_sql_risk)
