# pa-test-discover

Faux preprocessor in the style of `hspec-discover`, `tasty-discover` or `htfpp`
that automatically generates an entry point module for `mainserv`'s test suite.

It creates a `Main` module (unless otherwise told so via the `-m` flag) for an
`hspec` test suite that calls various test cases of type `Spec` that are found
in the (adjacent) module hierarchy. Tests are identified by the `test_` prefix.

As opposed to many other test preprocessing tools, the idea behind
`pa-test-discover` is that tests are defined alongside the normal library code.
Consequently, the test suite is a regular module of the library as well.

## Example usage

Consider a library with the following module hierarchy:

- `Foo` (has `test_foo`)
  - `Foo.Bar` (has `test_fooBar1` and `test_fooBar2`)
- `Baz` (has `test_baz`)

To add a `pa-test-discover`-based test suite, we'll add a `Spec` module with the
following content:

```haskell
{-# OPTIONS_GHC -F -pgmF pa-test-discover -optF -mSpec #-}
```

`-m` is necessary to set the module name (it isn't trying to be smart about this
at the moment). Due to the limitation that `pa-test-discover` currently only
searches for modules in (and below) the directory it is defined in, the `Spec`
module needs to be at the root of the library's module hierarchy.

To make the test module integrate better with Cabal's test running feature, we
can addtionally define a test suite depending on the main library with the
following `Main.hs`:

```haskell
module Main (main) where
import Spec (main)
```

Running the test suite then should give the following output:

```
Foo
  foo
Foo.Bar
  fooBar1
  fooBar2
Baz
  baz
```

As you can see, `pa-test-discover` automatically prints the module the test
belongs to and the test's name sans the `test_` prefix. The intention behind
this is to be able to match a function and its test name: If we want to test
`myFunction`, we can define a `test_myFunction` next to it and the test suite
output will be labeled correctly.
