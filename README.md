# wzMisc
[![Build Status](https://travis-ci.org/slin30/wzMisc.svg?branch=master)](https://travis-ci.org/slin30/wzMisc)  


## Miscellaneous functions package
*Version `1.1.3`*

This is the initial major release suitable for public consumption. 

## Changelog
`1.1.3 (2018-10-30)`
* Properly handle zero start for `make_chunks`

`1.1.2 (2018-10-10)`
* Handle multiple `class` returns in `profile_table`

`1.1.1 (2018-09-26)`
* Handle all NA columns for UNIQUEN and INTEGRAL_DUPE_FCTR in `profile_table` 

`1.1.0 (2018-09-26)`

* Add `profile_table`

---

## Installation

Install using devtools:

`devtools::install_github("slin30/wzMisc")`

## Note

Some functions in this package depend on data.table; please refer to their extensive wiki
for additional information:

[data.table installation](https://github.com/Rdatatable/data.table/wiki)
