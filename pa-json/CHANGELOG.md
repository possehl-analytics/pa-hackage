# Revision history for pa-json

## 0.3.0.0 -- 2023-10-15

- rename `jsonArray` to `mkJsonArray`
- add `toParseJson`, `toParseJSONErrorTree`, `asErrorTree`
- add `asArraySet`, `asObjectMap`, `countArrayElements`, `asUtcTime`, `asUtcTimeLenient`

## 0.2.1.0 -- 2023-06-29

- `Json.Enc`: Add `base64` and `base64Bytes`
- `Json.Enc`: Improve docs

## 0.2.0.0 -- 2023-06-15

- `Json.Enc`: Add `Show` instance which pretty-prints to a json string
- `Json.Enc`: Add encoding functions
- `Json.Enc.object`: Keep first element on duplicate keys instead of second
- `Json.Enc.object`: Keep order of elements (go through list instead of map)

## 0.1.0.0 -- 2023-05-24

- First version
