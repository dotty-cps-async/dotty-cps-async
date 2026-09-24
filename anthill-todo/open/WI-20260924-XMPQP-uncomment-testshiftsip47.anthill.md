## Attributes

- id: WI-20260924-XMPQP-uncomment-testshiftsip47
- created: 2026-09-24T09:03:19Z

- status: Open
- status_agent: user
- status_at: 2026-09-24T09:03:19Z

- acceptance: sbt-test

- tags: scala-3.9

## Description

Uncomment TestShiftSIP47 (clause interleaving)

shared/src/test/scala/cpstest/TestShiftSIP47.scala is fully commented out with a note "will be uncomment with scala-3.7". Clause interleaving (SIP-47, standard since 3.6) works inside async on 3.9.0: `method[A](using String)[B](await(..), await(..))` and `pure[Int](await(..))[String](await(..))` were probed on 2026-09-24. When enabling, drop the `given DebugLevel(20)` in the test (noisy output).

