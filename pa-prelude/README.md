# pa-prelude / `PossehlAnalyticsPrelude`

A prelude, we use in our projects; it should be imported in addition to `Prelude` from `base`,
because we don’t want to diverge from `base` too much.

Instead, we depend of `hlint` to rewrite/deny the use of some functions. See [../.hlint.yaml](../.hlint.yaml) for our hlint rules.
