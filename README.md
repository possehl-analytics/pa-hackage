# PA hackage packages

Some generic packages/modules that we use internally but want to provide to the Haskell community for dissection/use.

These are mostly integrations of different libraries that we have found to be useful and missing from hackage, or some small modules that we have found improve the way to write Haskell code.

Every package is prefixed with `pa-` on hackage, as to not clobber the hackage namespace. If we find a package to be very generally useful, we might remove the prefix.

All of these come as-is and provide no real stability guarantees (we might change them as we refactor internal code), though we will try to follow PVP/Semver.

## Maintainers

Duplication is handled via a `ninja` file; run `ninja` to keep everything up-to-date.

All package metadata is in [./project.json](./project.json) and is used to generate duplicated information.

### Creating a package

(this might go out of date)

1. Create a new package entry in [./project.json](./project.json).
2. Go to [./build.ninja](./build.ninja) and copy the block from another package (replacing the package name), and don’t forget to add the new package as input to `build-all`.
3. Run `ninja`
4. Add sources in `src`, write the `README` and add something to the changelog (copy from somewhere)
