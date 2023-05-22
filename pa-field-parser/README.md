# pa-field-parser

A small library for “vertical parsing” of values.

Traditional parsers are “horizontal” parsers, they take a flat list of tokens and produce structure:

<pre><code>
[  token1 token2 token3 token4 token5 ]
----> horizontal parser

result:

- token1
|
`-- token2
|
`-- token3
  |
  `-- token 4 -- token 5
</code></pre>

A `FieldParser` is a “vertical” parser. Once you have some low-level type in hand, usually you want to do some more checks, to “upgrade” it so to say:

<pre><code>
  Integer
    ^
    | signedDecimal
    |
   Text
    ^
    | utf8
    |
ByteString
</code></pre>

As a `FieldParser`, this would look like:

```haskell
utf8 :: FieldParser ByteString Text
signedDecimal :: FieldParser Text Integer

(utf8 >>> signedDecimal) :: FieldParser ByteString Integer
```

`>>>` is from `Control.Category`, but `Profunctor` is also available to map over the left and right arguments.

When run, this produces either a value or a helpful error message.

They can be freely combined with other libraries, and act as a nice adapter between them. For example, the JSON-related functions integrate with `aeson-better-errors` and any `FieldParser Value a` can be converted to a `FromJSON` instance for `aeson`. `attoparsec` is also available to easily turn bytes or text parsers into `FieldParser`s.

You can use this library as-is, but the design is easily adaptable to your codebase, the idea is very simple. Feel free to copy and paste what you need.
