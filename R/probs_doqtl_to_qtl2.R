#' Convert DOQTL genotype probabilities to R/qtl2 format
#'
#' Convert DOQTL genotype probabilities to R/qtl2 format
#'
#' @md
#'
#' @param probs 3d array of genotype probabilities as calculated from DOQTL
#'
#' @param map Data frame with marker map
#'
#' @param chr_column Name of the column in `map` that contains the chromosome IDs.
#'
#' @param pos_column Name of the column in `map` that contains the marker positions.
#'
#' @param marker_column Name of the column in `map` that contains
#' the marker names. If NULL, use the row names.
#'
#' @return An object of the form produced by [qtl2geno::calc_genoprob()].
#'
#' @export
probs_doqtl_to_qtl2 <-
    function(probs, map, chr_column="chr", pos_column="cM", marker_column="marker")
{
    if(is.null(marker_column)) {
        marker_column <- "qtl2tmp_marker"
        map[,marker_column] <- rownames(marker_column)
    }
    if(!(marker_column %in% colnames(map)))
        stop('Column "', marker_column, '" not found.')
    if(!(chr_column %in% colnames(map)))
        stop('Column "', chr_column, '" not found.')
    if(!(pos_column %in% colnames(map)))
        stop('Column "', pos_column, '" not found.')

    marker <- map[,marker_column]

    # subset map to the portion in probs
    map <- map[marker %in% dimnames(probs)[[3]],]
    marker <- map[,marker_column]

    # subset probs to the portion in map
    in_map <- dimnames(probs)[[3]] %in% marker
    if(any(!in_map)) {
        probs <- probs[,,in_map, drop=FALSE]
        warning("Omitting ", sum(!in_map), " markers from probs, not found in map")
    }

    # make sure they're aligned
    m <- match(marker, dimnames(probs)[[3]])
    probs <- probs[,,m, drop=FALSE]

    # reorder from (AA, AB, AC, AD, AE, ...) to (AA, AB, BB, AC, BC, CC, AD, ...)
    new_geno_order <- c(1L, 2L, 9L, 3L, 10L, 16L, 4L, 11L, 17L, 22L, 5L, 12L, 18L,
                        23L, 27L, 6L, 13L, 19L, 24L, 28L, 31L, 7L, 14L, 20L, 25L, 29L,
                        32L, 34L, 8L, 15L, 21L, 26L, 30L, 33L, 35L, 36L)

    # split probs (and reorder if necessary)
    chr <- map[,chr_column]
    uchr <- unique(chr)
    newprobs <- vector("list", length(uchr))
    names(newprobs) <- uchr
    for(i in uchr) {
        newprobs[[i]] <- probs[,,chr==i]
        if(ncol(newprobs[[i]])==36) { # reorder them
            newprobs[[i]] <- newprobs[[i]][,new_geno_order,,drop=FALSE]
        }
    }
    probs <- newprobs
    rm(newprobs)

    is_x_chr <- (uchr=="X")
    names(is_x_chr) <- uchr

    alleles <- LETTERS[1:8]
    if(ncol(probs[[1]])==8) {
        alleleprobs <- TRUE
    } else {
        alleleprobs <- FALSE
    }

    attr(probs, "is_x_chr") <- is_x_chr
    attr(probs, "crosstype") <- "do"
    attr(probs, "alleles") <- LETTERS[1:8]
    attr(probs, "alleleprobs") <- alleleprobs

    class(probs) <- c("calc_genoprob", "list")
    probs
}
