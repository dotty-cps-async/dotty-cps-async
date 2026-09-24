## Attributes

- id: WI-20260924-43BCA-report-scala3-bug-quotes
- created: 2026-09-24T09:03:19Z

- status: Open
- status_agent: user
- status_at: 2026-09-24T09:03:19Z

- acceptance: sbt-test

- tags: scala-3.9

## Description

Report scala3 bug: Quotes TreeMap fails on named patterns (case C(name = p))

A named pattern is typed as `Unapply(fun, Nil, List(Wildcard(), NamedArg("age", Bind("a", Wildcard()))))`; the default `quotes.reflect.TreeMap` dispatches NamedArg as a term and `transformTerm` fails with `scala.MatchError: Bind("a", Wildcard())`. Reproduced with 3.7.4, 3.8.4, 3.9.0; no existing scala/scala3 issue found (2026-09-24).
Reproducer: tests-cli/t2026_09_24_named_pattern_treemap (identity TreeMap only, added in PR #141). Workaround in dotty-cps-async: TransformUtil.transformCaseDef / transformPattern (PR #141).
After filing, reference the issue number in the tests-cli Test.scala and in the TransformUtil.transformPattern scaladoc.

