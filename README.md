quoted-printable
===

A quoted-printable decoder and encoder.

API
---
quoted-printable exposes just two functions at the moment:

```rust
    decode(&str, ParseMode) -> Result<Vec<u8>, QuotedPrintableError>
    encode(&[u8]) -> Vec<u8>
```

The decode function can be used to convert a quoted-printable string into the decoded bytes, as per the description in [IETF RFC 2045, section 6.7](https://tools.ietf.org/html/rfc2045#section-6.7).
The ParseMode option can be used to control whether the decoding is "strict" or "robust", as per the comments in that RFC.
In general you should probably use "robust" decoding, as it will gracefully handle more malformed input.

The encode function obviously does the reverse, and converts a set of raw bytes into quoted-printable.

Documentation
---
See the rustdoc at [http://staktrace.github.io/quoted-printable/quoted_printable/](http://staktrace.github.io/quoted-printable/quoted_printable/).

Other notes
---
This is written by a newbie Rust programmer, so code may be non-idiomatic or suboptimal. Pull requests are welcome!
