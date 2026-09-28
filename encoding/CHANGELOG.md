Change log for `crypton-asn1-encoding`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

* Decode and encode several times faster, allocating far less, with the same
  results as before (including for malformed input, and when and what a
  decoded list throws):
  * a stream is decoded straight from a strict bytestring: its structure is
    checked in one pass, then values are produced as they are asked for,
    without the intermediate list of events;
  * `encodeASN1 DER` and `toByteString` write straight into one buffer;
  * integers, OIDs and the time forms DER requires are converted without
    intermediate lists, and integers in time proportional to their size
    rather than to its square.

## 0.10.0 - 2025-08-01

* Depend on package `time-hourglass`, rather than `hourglass`. The fields of
  the `ASNTime` data constructor of data type `ASN1` are now types provided by
  the former package.
* Drop module `Data.ASN1.Object`, deprecated since `asn1-encoding-0.8.0`. Use
  module `Data.ASN1.Types`.
* Reexport module `Data.ASN1.Types` from package `crypton-asn1-types`.
* Drop unused dependency on `mtl` by test suite.

## 0.9.7 - 2025-07-20

* Move library modules to directory `src`.
* Drop support for GHC < 8.8.
* Use `LANAGUAGE RankNTypes`, rather than `LANGUAGE Rank2Types`.
* Use the `other-extensions` field in the Cabal file.
* Module `Data.ASN1.Stream` spun out to dependency package
  `crypton-asn1-types-0.3.6`.

## 0.9.6 - 2025-07-13

* Rename `asn1-encoding-0.9.6` package as `crypton-asn1-encoding-0.9.6`.
* Depend on `crypton-asn1-types`, rather than `asn1-types`.
* Change maintainer field to `Mike Pilgrem <public@pilgrem.com>` and
  `Kazu Yamamoto <kazu@iij.ad.jp>`.
* Add `CHANGELOG.md`.
* Cabal file specifies `cabal-version: 1.18` (not `>= 1.10`).
