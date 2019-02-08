# scan_qtl2_to_qtl
#
#' Convert scan1 results to the scanone format
#'
#' Convert the results of [qtl2::scan1()]
#' to the form used by the R/qtl function
#' [qtl::scanone()].
#'
#' @md
#'
#' @param scan1_output Matrix of LOD scores, as calculated by
#' [qtl2::scan1()].
#' @param map Map of markers/pseudomarkers (as a list of vectors).
#'
#' @return A data frame with class `"scanone"`, containing
#' chromosome and position columns followed by the LOD scores in
#' `scan1_output`.
#'
#' @examples
#' library(qtl2)
#' iron <- read_cross2(system.file("extdata", "iron.zip", package="qtl2"))
#' map <- insert_pseudomarkers(iron$gmap, step=1)
#' probs <- calc_genoprob(iron, map, error_prob=0.002)
#' pheno <- iron$pheno
#' covar <- match(iron$covar$sex, c("f", "m")) # make numeric
#' names(covar) <- rownames(iron$covar)
#' Xcovar <- get_x_covar(iron)
#' out <- scan1(probs, pheno, addcovar=covar, Xcovar=Xcovar)
#'
#' out_rev <- scan_qtl2_to_qtl(out, map)
#'
#' @seealso [scan_qtl2_to_qtl()]
#'
#' @export
scan_qtl2_to_qtl <-
    function(scan1_output, map)
{
    n <- vapply(map, length, 1)
    if(nrow(scan1_output) != sum(n))
        stop("nrow(scan1_output) [", nrow(scan1_output), "] != number of positions in map [",
             sum(n), "]")

    out <- data.frame(chr=factor(rep(names(map), n), levels=names(map)),
                      pos=unlist(map),
                      as.data.frame(unclass(scan1_output)))

    rownames(out) <- rownames(scan1_output)

    class(out) <- c("scanone", "data.frame")
    out
}
