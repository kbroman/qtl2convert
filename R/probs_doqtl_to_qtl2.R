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
    mat <- rbind(rep(1:8, each=8), rep(1:8, 8))
    old_geno_labels <- apply(mat[,mat[1,] <= mat[2,]], 2, function(a) paste(LETTERS[a], collapse=""))

    new_geno_labels <- c("AA", "AB", "BB", "AC", "BC", "CC", "AD", "BD", "CD", "DD", "AE", "BE",
                         "CE", "DE", "EE", "AF", "BF", "CF", "DF", "EF", "FF", "AG", "BG", "CG",
                         "DG", "EG", "FG", "GG", "AH", "BH", "CH", "DH", "EH", "FH", "GH", "HH")

    new_geno_order <- match(new_geno_labels, old_geno_labels)

    n_geno <- ncol(probs)

    # split probs (and reorder if necessary)
    chr <- map[,chr_column]
    if(is.factor(chr)) uchr <- levels(chr)
    else uchr <- unique(chr)

    newprobs <- setNames(vector("list", length(uchr)), uchr)
    for(i in uchr) {
        if(n_geno == 36) {
            newprobs[[i]] <- probs[,new_geno_order,chr==i]
        } else {
            newprobs[[i]] <- probs[,, chr==i]
        }
    }

    is_x_chr <- (uchr=="X")
    names(is_x_chr) <- uchr

    # need to fix X chr probabilities, but we don't know individuals sex
    # ... need to add it as an input


    if(n_geno == 8) {
        alleleprobs <- TRUE
    } else {
        alleleprobs <- FALSE
    }

    attr(newprobs, "is_x_chr") <- is_x_chr
    attr(newprobs, "crosstype") <- "do"
    attr(newprobs, "alleles") <- LETTERS[1:8]
    attr(newprobs, "alleleprobs") <- alleleprobs

    class(newprobs) <- c("calc_genoprob", "list")
    newprobs
}
