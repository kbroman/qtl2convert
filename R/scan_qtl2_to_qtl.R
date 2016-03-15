# scan_qtl2_to_qtl
#
#' Convert scan1 results to the scanone format
#'
#' Convert the results of \code{qtl2::scan1}
#' to the form used by the R/qtl function
#' \code{qtl::scanone}.
#'
#' @param scan_output Matrix of LOD scores, as calculated by
#' \code{qtl2::scan1}.
#'
#' @return A data frame with class \code{"scanone"}, containing
#' chromosome and position columns followed by the LOD scores in
#' \code{scan_output}.
#'
#' @examples
#' library(qtl2geno)
#' iron <- read_cross2(system.file("extdata", "iron.zip", package="qtl2geno"))
#' probs <- calc_genoprob(iron, step=1, error_prob=0.002)
#' pheno <- iron$pheno
#' covar <- match(iron$covar$sex, c("f", "m")) # make numeric
#' names(covar) <- rownames(iron$covar)
#' Xcovar <- get_x_covar(iron)
#' library(qtl2scan)
#' out <- scan1(probs, pheno, addcovar=covar, Xcovar=Xcovar)
#'
#' out_rev <- scan_qtl2_to_qtl(out)
#'
#' @export
scan_qtl2_to_qtl <-
    function(scan_output)
{
    map <- scan_output$map
    n <- sapply(map, length)

    # to handle scan_output of either scan1() or scan1coef()
    if("lod" %in% names(scan_output))
        lod <- scan_output$lod
    else if("coef" %in% names(scan_output))
        lod <- scan_output$coef
    else stop("Neither lod nor coef found.")

    stopifnot(sum(n) == nrow(lod))

    out <- data.frame(chr=factor(rep(names(map), n), levels=names(map)),
                      pos=unlist(map),
                      as.data.frame(lod))

    rownames(out) <- rownames(scan_output$lod)

    class(out) <- c("scanone", "data.frame")
    out
}