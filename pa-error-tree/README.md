# pa-error-tree

Oftentimes you want to validate a bunch of things, recursively.

In that case, stopping at the first error is not the best thing to do, instead you’d want to accumulate errors and print the whole tree of problems in one go (e.g. when validating some input).

This library provides a simple wrapper around `Data.Tree` and `Error` which can be combined with `Validation (NonEmpty Error)` and `Validation (NonEmpty ErrorTree)` to elegantly collect and annotate errors recursively.

Note that it only deals with errors that should be displayed to other programmers, and only error strings. If you need to validate and display (structured) user errors, this is not it.
