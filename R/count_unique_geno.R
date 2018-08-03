# count_unique_geno
#' Count the unique genotypes for each row of a genotype matrix
#'
#' For genotype data (markers x individuals) on a set of individuals,
#' count the unique genotypes for each marker
#'
#' @md
#'
#' @param genotypes Matrix of genotypes (markers x individuals)
#' @param na.strings Genotypes to be considered as missing values.
#'
#' @return Vector of counts of unique genotypes.
#'
#' @seealso [find_unique_geno()]
#' @export
#' @examples
#' g <- rbind(c("NA", "A",  "A",  "A", "T"),
#'            c("NA", "NA", "NA", "A", "A"),
#'            c("A",  "A",  "T",  "G", "G"),
#'            c("C", "C",  "G",  "G", "NA"))
#' count_unique_geno(g)
count_unique_geno <-
function(genotypes, na.strings=c("N", "H", "NA", ""))
{
    genotypes[is.na(genotypes) | (genotypes %in% na.strings)]  <- "NA"

    # force to be a matrix
    if(!is.matrix(genotypes)) genotypes <- as.matrix(genotypes)

    result <- .count_unique_geno(genotypes)
    names(result) <- rownames(genotypes)

    result
}
