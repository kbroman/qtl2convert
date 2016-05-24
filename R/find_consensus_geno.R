# find_consensus_geno
#' Find the consensus genotype for each row of a genotype matrix
#'
#' For genotype data (markers x individuals) on a set of individuals
#' from a single inbred line, find the consensus genotype at each
#' marker.
#'
#' @param genotypes Matrix of genotypes (markers x individuals)
#'
#' @return Vector of consensus genotypes, one value per row of \code{genotypes}
#'
#' @details We take \code{"NA"}, \code{"N"}, and \code{"H"} as
#' missing. Internally (in the C++ code), we use \code{"NA"} as the
#' missing value.
#'
#' @export
#' @examples
#' g <- rbind(c("NA", "N", "A", "A", "T", "G", NA, "H"),
#'            c("C",  "C", "G", "G", "A", NA,  NA, NA),
#'            rep(NA, 8),
#'            c("C", "C", "G", "G", "G", "C", "G", "G"))
#' find_consensus_geno(g)
find_consensus_geno <-
function(genotypes)
{
    genotypes[is.na(genotypes) | genotypes=="N" | genotypes=="H"] <- "NA"

    result <- .find_consensus_geno(genotypes)
    result[result=="NA"] <- NA
    names(result) <- rownames(genotypes)

    result
}
