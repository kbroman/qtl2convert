#' Convert R/qtl genotype probabilities to R/qtl2 format
#'
#' Convert R/qtl genotype probabilities to R/qtl2 format
#'
#' @param cross An R/qtl \code{"cross"} object (see
#'     \code{\link[qtl]{read.cross} for details.} Must contain
#'     genotype probabilities as calculated by
#'     \code{\link[qtl]{calc.genoprob}}.
#'
#' @return A list with two components:
#' - \code{"probs"} - the genotype probabilities in the form produced by \code{\link[qtl2geno]{calc_genoprob}}
#' - \code{"map"} - Map of marker/pseudomarker positions (a list of vectors of positions)
#'
#' @examples
#' library(qtl)
#' data(hyper)
#' hyper <- calc.genoprob(hyper, step=1, error.prob=0.002)
#' result <- probs_qtl_to_qtl2(hyper)
#' pr <- result$probs
#' map <- result$map
#'
#' @importFrom stats setNames
#' @export
probs_qtl_to_qtl2 <-
    function(cross)
{
    if(!("cross" %in% class(cross)))
        stop("Input must be an R/qtl cross object")
    if(!("prob" %in% names(cross$geno[[1]])))
        stop("Input doesn't contain genotype probabilities from calc.genoprob")

    crosstype <- class(cross)[1]
    chr <- names(cross$geno)

    # chromosome types -> is_x_chr
    chrtype <- sapply(cross$geno, class)
    chrtype[chrtype != "A" & chrtype != "X"] <- "A"
    is_x_chr <- stats::setNames(chrtype=="X", chr)

    # grab probabilities
    pr <- lapply(cross$geno, function(a) a$prob)

    # fix X chr if necessary
    if(any(is_x_chr) && (crosstype=="bc" || crosstype=="f2" || crosstype=="bcsft")) {
        sexpgm <- qtl::getsex(cross)
        for(j in seq_along(cross$geno)[is_x_chr])
            pr[[j]] <- qtl::reviseXdata(crosstype, "full", sexpgm, prob=pr[[j]],
                                        cross.attr=attributes(cross), force=TRUE)
    }

    # reorder dimensions of the probabilities (ind,pos,genotype) -> (ind,genotype,pos)
    pr <- lapply(pr, function(a) aperm(a, c(1,3,2)))

    # grab map
    map <- lapply(cross$geno, function(a) attr(a$prob, "map"))

    # add chromosome names + individual IDs
    names(pr) <- names(map) <- chr
    ids <- getid(cross)
    if(is.null(ids)) ids <- 1:qtl::nind(cross)
    for(i in seq_along(pr)) rownames(pr[[i]]) <- ids

    attr(pr, "crosstype") <- crosstype
    attr(pr, "is_x_chr") <- is_x_chr
    attr(pr, "alleles") <- attr(cross, "alleles")
    attr(pr, "alleleprobs") <- FALSE
    class(pr) <- "calc_genoprob"

    list(probs=pr, map=map)
}
