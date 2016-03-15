#' Convert DOQTL genotype probabilities to R/qtl2 format
#'
#' Convert DOQTL genotype probabilities to R/qtl2 format
#'
#' @param probs 3d array of genotype probabilities as calculated from DOQTL
#'
#' @param map Data frame with marker map
#'
#' @param chr_column Name of the column in \code{map} that contains the chromosome IDs.
#'
#' @param pos_column Name of the column in \code{map} that contains the marker positions.
#'
#' @param marker_column Name of the column in \code{map} that contains
#' the marker names. If NULL, use the row names.
#'
#' @return An object of the for produced by the R/qtl2geno function \code{calc_genoprob}.
#'
#' @export
probs_doqtl_to_qtl2 <-
    function(probs, map, chr_column="chr", pos_column="cM", marker_column="marker")
{
    if(is.null(marker_column)) {
        marker_column <- "qtl2tmp_marker"
        map[,marker_column] <- rownames(marker_column)
    }
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

    # split probs
    chr <- map[,chr_column]
    uchr <- unique(chr)
    newprobs <- vector("list", length(uchr))
    names(newprobs) <- uchr
    for(i in uchr) newprobs[[i]] <- probs[,,chr==i]
    probs <- newprobs
    rm(newprobs)

    # convert map to list
    map <- map_df_to_list(map, chr_column, pos_column, marker_column)

    is_x_chr <- (chr=="X")
    names(is_x_chr) <- chr

    alleles <- LETTERS[1:8]
    if(ncol(probs[[1]])==8)
        alleleprobs <- TRUE
    else {
        alleleprobs <- FALSE
        warning("Order of genotypes may be incorrect.") ## FIX_ME
    }

    result <- list(probs=probs,
                   map=map,
                   is_x_chr=is_x_chr,
                   crosstype="do",
                   alleles=LETTERS[1:8],
                   alleleprobs=alleleprobs)
    # missing sex and cross_info

    class(result) <- c("calc_genoprob", "list")
    result
}
