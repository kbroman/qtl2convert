# find_unique_geno
#' Find the unique genotypes for each row of a genotype matrix
#'
#' For genotype data (markers x individuals) on a set of individuals,
#' find the unique genotypes for each marker, provided that there are exactly two.
#' (If more than two or fewer than two, return NAs.)
#'
#' @param genotypes Matrix of genotypes (markers x individuals)
#' @param na.strings Genotypes to be considered as missing values.
#'
#' @return Matrix with two columns. Each row corresponds to a marker,
#' and has the two unique genotypes, or \code{NA}s (if >2 or <2 unique
#' genotypes).
#'
#' @seealso \code{\link{count_unique_geno}}, \code{\link{encode_geno}}
#' @export
#' @examples
#' g <- rbind(c("NA", "A",  "A",  "A", "T"),
#'            c("NA", "NA", "NA", "A", "A"),
#'            c("A",  "A",  "T",  "G", "G"),
#'            c("C", "C",  "G",  "G", "NA"))
#' find_unique_geno(g)
find_unique_geno <-
function(genotypes, na.strings=c("N", "H", "NA", ""))
{
    genotypes[is.na(genotypes) | (genotypes %in% na.strings)]  <- "NA"

    result <- .find_unique_geno(genotypes)
    result[result=="NA"] <- NA
    rownames(result) <- rownames(genotypes)

    result
}
