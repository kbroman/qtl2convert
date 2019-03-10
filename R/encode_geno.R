# encode_geno
#' Encode a matrix of genotypes using a set of allele codes
#'
#' Encode a matrix of genotypes using a set of allele codes.
#'
#' @param geno Character matrix of genotypes (rows as markers, columns as individuals)
#' @param allele_codes Two-column matrix of alleles (rows as markers)
#' @param output_codes Vector of length four, with missing, AA, AB, BB codes
#' @param cores Number of CPU cores to use, for parallel calculations.
#' (If `0`, use [parallel::detectCores()].)
#' Alternatively, this can be links to a set of cluster sockets, as
#' produced by [parallel::makeCluster()].
#'
#' @return Matrix of same dimensions as `geno`, but with values in `output_codes`.
#'
#' @seealso [find_consensus_geno()], [find_unique_geno()]
#'
#' @export
#' @importFrom qtl2 batch_vec
#'
#' @examples
#' geno <- rbind(c("C", "G", "C",  "GG", "CG"),
#'               c("A", "A", "AT", "TA", "TT"),
#'               c("T", "G", NA,   "GT", "TT"))
#' codes <- rbind(c("C", "G"), c("A", "T"), c("T", "G"))
#' encode_geno(geno, codes)
#'
encode_geno <-
function(geno, allele_codes, output_codes=c("-", "A", "H", "B"), cores=1)
{
    stopifnot(nrow(geno) == nrow(allele_codes))
    stopifnot(ncol(allele_codes) == 2)

    if(!is.matrix(geno)) geno <- as.matrix(geno)
    if(!is.matrix(allele_codes)) allele_codes <- as.matrix(allele_codes)

    # if no rows, nothing to do
    if(nrow(geno)==0) return(geno)

    geno_codes <- cbind(allele_codes, # A and B
                        paste0(allele_codes[,1], allele_codes[,1]), # AA
                        paste0(allele_codes[,2], allele_codes[,2]), # BB
                        paste0(allele_codes[,1], allele_codes[,2]), # AB
                        paste0(allele_codes[,2], allele_codes[,1])) # BA

    cores <- setup_cluster(cores, quiet=TRUE)
    by_batch_func <- function(batch) .encode_geno(geno[batch,,drop=FALSE], geno_codes[batch,,drop=FALSE],
                                                  output_codes[c(2,4,2,4,3,3)])
    batches <- batch_vec(1:nrow(geno), n_cores=n_cores(cores))

    result <- cluster_lapply(cores, batches, by_batch_func)
    result <- do.call("rbind", result)
    result[is.na(result)] <- output_codes[1]

    dimnames(result) <- dimnames(geno)
    result
}
