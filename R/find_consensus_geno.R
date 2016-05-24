# find_consensus_geno
#' Find the consensus genotype for each row of a genotype matrix
#'
#' For genotype data (markers x individuals) on a set of individuals
#' from a single inbred line, find the consensus genotype at each
#' marker.
#'
#' @param genotypes Matrix of genotypes (markers x individuals)
#' @param na.strings Genotypes to be considered as missing values.
#'
#' @return Vector of consensus genotypes, one value per row of \code{genotypes}
#'
#' @export
#' @examples
#' g <- rbind(c("NA", "N", "A", "A", "T", "G", NA, "H"),
#'            c("C",  "C", "G", "G", "A", NA,  NA, NA),
#'            rep(NA, 8),
#'            c("C", "C", "G", "G", "G", "C", "G", "G"))
#' find_consensus_geno(g)
find_consensus_geno <-
function(genotypes, na.strings=c("N", "H", "NA", ""))
{
    genotypes[is.na(genotypes) | (genotypes %in% na.strings)]  <- "NA"

    result <- .find_consensus_geno(genotypes)
    result[result=="NA"] <- NA
    names(result) <- rownames(genotypes)

    result
}
