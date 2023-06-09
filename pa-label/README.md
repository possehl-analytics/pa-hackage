# `pa-label`

One-module library to provide nice anonymous labelled tuples and enums.

If you have a function:

```
doBlorb
  :: Text
  -> Text
  -> Bool
  -> Bool
  -> Bool
  -> IO Text
doBlorb username password read write insert = do …
```

what do you do? At call-site you don’t see the names of any of the types.

Before GHC 9.2, one strategy would have been to create a newtype for every single argument:

```
data Username = Username Text
data Password = Password Text
data Read = Read Bool
data Write = Write Bool
data Insert = Insert Bool
```

but that is very verbose and leaks into global scope!

Instead, do this:

```
doBlorb
  :: Label "username" Text
  -> Label "password" Text
  -> Label "read" Bool
  -> Label "write" Bool
  -> Label "insert" Bool
  -> IO (Label "blorb" Text)
doBlorb username password read write insert = do …
```

then it becomes clear to the call-site, what to do. In the function you can access the labels with record dots to get to the inner types, e.g. `username.username`, `read.read`.

Even better, you can bundle things into anonymous tuples:

```
doBlorb
  :: T2 "username" Text
        "password" Text
  -> T3 "read" Bool
        "write" Bool
        "insert" Bool
  -> IO (Label "blorb" Text)
doBlorb user permission = do …
```

and access inside the function like `user.username` or `permission.read`. So much nicer!

We provide tuples up to size 3, for anything bigger the you should really just create a normal record with `data`. The great thing is that no use-site has to be adjusted as long as you name the record fields the same and use record-dot syntax everywhere.

There’s some experimental support for anonymous enums (`E2/E3`), they are useful in some situations, but not as often as anonymous tuples in our experience.

---

Approaches like [vinyl](https://hackage.haskell.org/package/vinyl) or [superrecord](https://hackage.haskell.org/package/superrecord) are more general, but very hard to understand and lead to bad error messages.

We have found that in practice it is nicer to use the a lot less magical and slightly more verbose `T2/T3` and switch to normal records once you reach 4 fields.
