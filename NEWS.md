## qtl2convert 0.19-1 (2019-03-10)

- Use Markdown for function documentation, throughout


## qtl2convert 0.18 (2019-02-08)

- Added `scan_qtl_to_qtl2()` to convert `scanone()` output from R/qtl
  into the new R/qtl2 `scan1()` format.

- Small changes to `find_unique_geno()`, `count_unique_geno()`, and
  `find_consensus_geno()` to allow input to be a data frame.

- Enable multi-core calculatings (adding `cores` argument) to
  `find_unique_geno()`, `count_unique_geno()`,
  `find_consensus_geno()`, and `encode_geno()`.

- Added `cbind_smother()` for combining matrices, but
  "smothering" columns in the first matrix by those in the second
  matrix that have the same name. Uses `qtl2::cbind_expand()`.


## qtl2convert 0.16 (2018-07-23)

- No real changes; just keeping in sync with the qtl2 package.


## qtl2convert 0.14 (2018-03-09)

- No real changes; just keeping in sync with the qtl2 package.


## qtl2convert 0.12 (2018-01-19)

- Again, no actual changes; just keeping in sync with the qtl2 package.


## qtl2convert 0.10 (2018-01-09)

- No actual changes; just keeping in sync with the qtl2 package.


## qtl2convert 0.8 (2018-01-05)

- First formal release


## qtl2convert 0.7-2 (2017-12-05)

### New features

- Added a new function `probs_qtl2_to_array()` for converting an
  R/qtl2 genotype probabilities object (well, just the autosomal part)
  into a three-dimensional array. This is particularly for use in
  comparing two sets of genotype probabilities to look for sample
  mix-ups (e.g., with the [R/lineup2](https://github.com/kbroman/lineup2)
  package.


## qtl2convert 0.7-1 (2017-11-27)

- Revised to go with the merge of qtl2geno, qtl2scan, qtl2plot, and
  qtl2db into a single package (R/qtl2)


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
