# encode_geno
#' Encode a matrix of genotypes using a set of allele codes
#'
#' Encode a matrix of genotypes using a set of allele codes.
#'
#' @param geno Character matrix of genotypes
#' @param allele_codes Two-column matrix of alleles
#' @param output_codes Vector of length four, with missing, AA, AB, BB codes
#'
#' @return Matrix of same dimensions as \code{geno}, but with values in \code{output_codes}.
#'
#' @seealso \code{\link{find_consensus_geno}}, \code{\link{find_unique_geno}}
#' @export
#' @examples
#' geno <- rbind(c("C", "G", "C",  "GG", "CG"),
#'               c("A", "A", "AT", "TA", "TT"),
#'               c("T", "G", NA,   "GT", "TT"))
#' codes <- rbind(c("C", "G"), c("A", "T"), c("T", "G"))
#' encode_geno(geno, codes)
#'
encode_geno <-
function(geno, allele_codes, output_codes=c("-", "A", "H", "B"))
{
    stopifnot(nrow(geno) == nrow(allele_codes))
    stopifnot(ncol(allele_codes) == 2)

    geno_codes <- cbind(allele_codes, # A and B
                        paste0(allele_codes[,1], allele_codes[,1]), # AA
                        paste0(allele_codes[,2], allele_codes[,2]), # BB
                        paste0(allele_codes[,1], allele_codes[,2]), # AB
                        paste0(allele_codes[,2], allele_codes[,1])) # BA

    result <- .encode_geno(geno, geno_codes, output_codes[c(2,4,2,4,3,3)])
    result[is.na(result)] <- output_codes[1]

    dimnames(result) <- dimnames(geno)
    result
}
