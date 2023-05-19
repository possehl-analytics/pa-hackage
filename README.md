# PA hackage packages

Some generic packages/modules that we use internally but want to provide to the Haskell community for dissection/use.

These are mostly integrations of different libraries that we have found to be useful and missing from hackage, or some small modules that we have found improve the way to write Haskell code.

Every package is prefixed with `pa-` on hackage, as to not clobber the hackage namespace. If we find a package to be very generally useful, we might remove the prefix.

All of these come as-is and provide no real stability guarantees (we might change them as we refactor internal code), though we will try to follow PVP/Semver.


## Maintainers

Duplication is handled via a `ninja` file; run `ninja` to keep everything up-to-date.
