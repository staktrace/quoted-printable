quoted-printable
===

A quoted-printable decoder (and eventually, hopefully, an encoder too).

API
---
quoted-printable exposes just one function at the moment:

```rust
    decode(&str, ParseMode) -> Result<Vec<u8>, QuotedPrintableError>
```

This function can be used to convert a quoted-printable string into the decoded bytes, as per the description in [IETF RFC 2045, section 6.7](https://tools.ietf.org/html/rfc2045#section-6.7).
The ParseMode option can be used to control whether the decoding is "strict" or "robust", as per the comments in that RFC.
In general you should probably use "robust" decoding, as it will gracefully handle more malformed input.

Other notes
---
This is written by a newbie Rust programmer, so code may be non-idiomatic or suboptimal. Pull requests are welcome!
