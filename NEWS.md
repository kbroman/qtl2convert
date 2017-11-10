## qtl2convert 0.5-10 (2017-11-09)

### New features

- Added function `probs_qtl2_to_doqtl()` for converting the
  `calc_genoprob()` output from the R/qtl2 format to a single big
  3-dimensional array, for use with DOQTL.


## qtl2convert 0.5-6 (2017-06-08)

### New features

- Revised `probs_doqtl_to_qtl2()` so that the genotypes are reordered
  appropriately. Still not sure about X chromosome, though.


## qtl2convert 0.5-5 (2017-06-05)

### Minor changes

- Revised installation instructions.


## qtl2convert 0.5-4 (2017-05-24)

### New features

- Added function `probs_qtl_to_qtl2` for converting genotype
  probabilities from R/qtl to R/qtl2 format.


## qtl2convert 0.5-3 (2017-04-29)

## Minor changes

- Add `overwrite` argument to `write2csv`. If `overwrite=TRUE`, write
  over file if it exists; if `overwrite=FALSE`, stop with an error.


## qtl2convert 0.5-2 (2017-04-19)

### Minor changes

- `encode_geno`: convert inputs to matrices if necessary.


## qtl2convert 0.5-1 (2017-03-13)

### New features

- Revised `probs_doqtl_to_qtl2` and `scan_qtl2_to_qtl` to deal with
  changes to R/qtl2 data structures in
  [qtl2geno](https://github.com/rqtl/qtl2geno) and
  [qtl2scan](https://github.com/rqtl/qtl2scan). Each function now
  needs a `map` object, generally created by
  `qtl2geno::insert_pseudomarkers()`.
