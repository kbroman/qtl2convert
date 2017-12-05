#' Convert R/qtl2 genotype probabilities to a 3d array
#'
#' Convert R/qtl2 genotype probabilities to a 3d array
#'
#' @md
#'
#' @param probs A `"calc_genoprob"` object (a list of 3d arrays of
#' genotype probabilities), as calculated by [qtl2::calc_genoprob()].
#'
#' @return A single three-dimensional array, with just the autosomal
#' genotype probabilities.
#'
#' @details
#' We convert just the autosomal genotype probabilities, because they
#' should all have the same number of genotypes (columns). The main
#' application of this is for identifying possible sample mix-ups
#' among batches of genotype probabilities (e.g., using the
#' [R/lineup2](https://github.com/kbroman/lineup2) package), and for
#' this the autosomal genotype probabilities should be sufficient.
#'
#' @importFrom stats setNames
#' @export
probs_qtl2_to_array <-
    function(probs)
{
    # which is the X chromosome?
    is_x_chr <- attr(probs, "is_x_chr")
    if(is.null(is_x_chr)) is_x_chr <- rep(FALSE, length(probs))

    # reduce to autosomes
    probs <- unclass(probs)[!is_x_chr]

    # dimensions
    d <- vapply(probs, dim, 1:3)

    if(length(unique(d[1,])) != 1)
        stop("Different chromosomes have different numbers of individuals")

    if(length(unique(d[2,])) != 1)
        stop("Different chromosomes have different numbers of genotypes")

    result <- array(dim=setNames(c(d[1,1], d[2,1], sum(d[3,])), NULL))
    dimnames(result) <- list(rownames(probs[[1]]),
                             colnames(probs[[1]]),
                             unlist(lapply(probs, function(a) dimnames(a)[[3]])))

    cur <- 0
    for(i in seq_along(probs)) {
        result[,,1:d[3,i]] <- probs[[i]]
        cur <- cur + d[3,i]
    }

    result
}
