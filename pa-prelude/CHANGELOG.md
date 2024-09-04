# Revision history for pa-prelude

## 0.3.0.0 -- 2024-09-04

- Add `IsEmpty` and `IsNonEmpty` patterns (for getting a `NonEmpty` from a match)
- Add `stringToBytesUtf8`
- Add multiple function for dealing with `NonEmpty`
- Add `lengthNatural` and a `Lengthy` class that should be preferred over `Foldable.length`
- Add `maximumBy1` and `minimumBy1`

## 0.2.0.0 -- 2023-10-15

- `thenValidate` is now `thenValidateM`
- added `thenValidate` as non-monadic `thenValidateM`

## 0.1.0.0 -- 2023-05-19

- First release
