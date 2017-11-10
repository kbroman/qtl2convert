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
#' @param is_female Optional logical vector indicating which
#' individuals are female. Names should contain the individual
#' identifiers, matching the row names in `probs`.
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
#' @details
#' We assume that the X chromosome is labeled `"X"` (must be
#' upper-case) and that any other chromosomes are autosomes.
#' We assume that the genotypes are labeled using the 8 letters A-H.
#'
#' If the probabilities are for the full 36 states and the X
#' chromosome is included but `is_female` is not provided, we'll guess
#' which individuals are females based on their genotype
#' probabilities. (If the average, across loci, of the sum of the
#' heterozygote probabilities is small, we'll assume it's a female.)
#'
#' @export
probs_doqtl_to_qtl2 <-
    function(probs, map, is_female=NULL, chr_column="chr", pos_column="cM", marker_column="marker")
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
    if(sum(in_map)==0) {
        stop("No markers in common between probs and map")
    }
    if(any(!in_map)) {
        probs <- probs[,,in_map, drop=FALSE]
        warning("Omitting ", sum(!in_map), " markers from probs, not found in map")
    }

    # make sure they're aligned
    m <- match(marker, dimnames(probs)[[3]])
    probs <- probs[,,m, drop=FALSE]

    # number of genotypes
    n_geno <- ncol(probs)
    if(n_geno != 36 & n_geno != 8)
        stop("number of genotypes (number of columns of probs) should be 8 or 36")

    # align probabilities and is_female
    if(!is.null(is_female) && n_geno==36) { # if n_geno==8, ignore is_female
        ids1 <- names(is_female)
        ids2 <- rownames(probs)
        if(is.null(ids1) || is.null(ids2))
            stop("probs and is_female must both contain individual identifiers")
        both <- ids2[ids2 %in% ids1] # take order from probs
        if(length(both) == 0)
            stop("No individuals in common between probs and is_female")
        n_sex_only <- sum(!(ids1 %in% both))
        n_probs_only <- sum(!(ids2 %in% both))
        if(n_sex_only > 0) {
            warning("Omitting ", n_sex_only, " individuals absent from probs")
        }
        is_female <- is_female[both] # reorder no matter what
        if(n_probs_only > 0) {
            warning("Omitting ", n_probs_only, " individuals absent from is_female")
            probs <- probs[both,,,drop=FALSE]
        }
    }

    # reorder from (AA, AB, AC, AD, AE, ...) to (AA, AB, BB, AC, BC, CC, AD, ...)
    mat <- rbind(rep(1:8, each=8), rep(1:8, 8))
    old_geno_labels <- apply(mat[,mat[1,] <= mat[2,]], 2, function(a) paste(LETTERS[a], collapse=""))

    new_geno_labels <- c("AA", "AB", "BB", "AC", "BC", "CC", "AD", "BD", "CD", "DD", "AE", "BE",
                         "CE", "DE", "EE", "AF", "BF", "CF", "DF", "EF", "FF", "AG", "BG", "CG",
                         "DG", "EG", "FG", "GG", "AH", "BH", "CH", "DH", "EH", "FH", "GH", "HH")

    new_geno_order <- match(new_geno_labels, old_geno_labels)

    # split probs (and reorder if necessary)
    chr <- map[,chr_column]
    if(is.factor(chr)) {
        chr <- factor(chr) # reduce to observed levels
        uchr <- levels(chr)
    }
    else uchr <- unique(chr)

    newprobs <- setNames(vector("list", length(uchr)), uchr)
    for(i in uchr) {
        if(n_geno == 36) {
            newprobs[[i]] <- probs[,new_geno_order,chr==i,drop=FALSE]
        } else {
            newprobs[[i]] <- probs[,, chr==i,drop=FALSE]
        }

        if(i == "X" && n_geno==36) { # need to pull apart the males and females
            newprobs[[i]] <- array(0, dim=c(nrow(probs), ncol(probs)+8, sum(chr==i)))
            dimnames(newprobs[[i]]) <- list(rownames(probs),
                                            c(new_geno_labels, paste0(LETTERS[1:8], "Y")),
                                            dimnames(probs)[[3]][chr==i])

            if(is.null(is_female)) { # need to guess sex
                het_tolerance <- 1e-4

                hom <- paste0(LETTERS[1:8], LETTERS[1:8])
                het <- old_geno_labels[!(old_geno_labels %in% hom)]
                sum_het <- apply(probs[,het,chr==i,drop=FALSE], 1, sum)/dim(probs)[[3]]
                is_female <- setNames((sum_het > het_tolerance), rownames(probs))
            }
            if(any(is_female)) {
                newprobs[[i]][is_female, 1:36, ] <- probs[is_female, new_geno_order, chr==i, drop=FALSE]
            }
            if(any(!is_female)) {
                newprobs[[i]][!is_female, 37:44, ] <- probs[!is_female, hom, chr==i, drop=FALSE]
            }
        }
    }

    is_x_chr <- (uchr=="X")
    names(is_x_chr) <- uchr

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
