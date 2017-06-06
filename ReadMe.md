### R/qtl2convert

[![Build Status](https://travis-ci.org/rqtl/qtl2convert.svg?branch=master)](https://travis-ci.org/rqtl/qtl2convert)

[Karl Broman](http://kbroman.org)

[R/qtl2convert](https://github.com/rqtl/qtl2convert) is an R package
for converting data among [R/qtl2](http://kbroman.org/qtl2),
[R/qtl](http://rqtl.org), and
[DOQTL](https://www.bioconductor.org/packages/release/bioc/html/DOQTL.html).

---

### Installation

R/qtl2 is not yet available on [CRAN](https://cran.r-project.org), but
it can be installed from a mini-CRAN at [rqtl.org](http://rqtl.org).

    install.packages("qtl2", repos="http://rqtl.org/qtl2cran")

The [qtl2](https://github.com/rqtl/qtl2) package is
inspired by the
[tidyverse package](https://cran.r-project.org/package=tidyverse);
it is basically empty, but when you install it, the
[qtl2geno](https://github.com/rqtl/qtl2geno),
[qtl2scan](https://github.com/rqtl/qtl2scan),
[qtl2plot](https://github.com/rqtl/qtl2plot), and
[qtl2convert](https://github.com/rqtl/qtl2convert) packages, plus a
bunch of dependencies, will be installed.

Alternatively, you can install R/qtl2 from its source on
[GitHub](https://github.com/rqtl). (But note that compiling the C++
code can be rather slow.)

On _Windows_, you'll need [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

On _Mac OS X_, you'll need the
[command-line developer tools](https://mac-how-to.gadgethacks.com/how-to/install-command-line-developer-tools-without-xcode-0168115/),
as well as [gfortran](https://gcc.gnu.org/wiki/GFortranBinaries#MacOS).

You then need to install the
[devtools](https://github.com/hadley/devtools) package, plus a set of
package dependencies: [yaml](https://cran.r-project.org/package=yaml),
[jsonlite](https://cran.r-project.org/package=jsonlite),
[data.table](https://cran.r-project.org/package=data.table),
and [RcppEigen](https://github.com/RcppCore/RcppEigen).
(Additional, secondary dependencies will also be installed.)

    install.packages(c("devtools", "yaml", "jsonlite", "data.table", "RcppEigen"))

Finally, install R/qtl2 using `devtools::install_github()`.

    library(devtools)
    install_github("rqtl/qtl2")

---

#### License

[Licensed](License.md) under [GPL-3](https://www.r-project.org/Licenses/GPL-3).
