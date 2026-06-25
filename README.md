### R/qtl2convert

[![R-CMD-check](https://github.com/rqtl/qtl2convert/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rqtl/qtl2convert/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/qtl2convert)](https://cran.r-project.org/package=qtl2convert)
[![r-universe badge](https://rqtl.r-universe.dev/qtl2convert/badges/version)](https://rqtl.r-universe.dev/qtl2convert)
[![zenodo DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3237774.svg)](https://doi.org/10.5281/zenodo.3237774)

[R/qtl2](https://kbroman.org/qtl2/) (aka qtl2) is a reimplementation of
the QTL analysis software [R/qtl](https://rqtl.org), to better handle
high-dimensional data and complex cross designs.

The [qtl2convert](https://github.com/rqtl/qtl2convert) package is
for converting data among the R/qtl2,
[DOQTL](https://www.bioconductor.org/packages/3.9/bioc/html/DOQTL.html),
and [R/qtl](https://rqtl.org) formats.

---

### Installation

Install the qtl2convert package from [CRAN](https://cran.r-project.org):

```r
install.packages("qtl2convert")
```

Alternatively, install it from [R
universe](https://rqtl.r-universe.dev):

```r
install.packages("qtl2convert", repos=c("https://rqtl.r-universe.dev",
                                        "https://cloud.r-project.org"))
```

Or use [remotes](https://remotes.r-lib.org) to install it from its GitHub source:

```r
install.packages("remotes")
remotes::install_github("rqtl/qtl2convert")
```

---

### License

Licensed under [GPL-3](https://www.r-project.org/Licenses/GPL-3).
