# find_consensus_geno
#' Find the consensus genotype for each row of a genotype matrix
#'
#' For genotype data (markers x individuals) on a set of individuals
#' from a single inbred line, find the consensus genotype at each
#' marker.
#'
#' @param genotypes Matrix of genotypes (markers x individuals)
#' @param na.strings Genotypes to be considered as missing values.
#' @param cores Number of CPU cores to use, for parallel calculations.
#' (If `0`, use [parallel::detectCores()].)
#' Alternatively, this can be links to a set of cluster sockets, as
#' produced by [parallel::makeCluster()].
#'
#' @return Vector of consensus genotypes, one value per row of `genotypes`
#'
#' @seealso [find_unique_geno()], [encode_geno()]
#'
#' @export
#' @importFrom qtl2 batch_vec
#'
#' @examples
#' g <- rbind(c("NA", "N", "A", "A", "T", "G", NA, "H"),
#'            c("C",  "C", "G", "G", "A", NA,  NA, NA),
#'            rep(NA, 8),
#'            c("C", "C", "G", "G", "G", "C", "G", "G"))
#' consensus <- find_consensus_geno(g)
find_consensus_geno <-
function(genotypes, na.strings=c("N", "H", "NA", ""), cores=1)
{
    genotypes[is.na(genotypes) | (genotypes %in% na.strings)]  <- "NA"

    if(!is.matrix(genotypes)) genotypes <- as.matrix(genotypes)

    # if no rows, return NULL
    if(nrow(genotypes)==0) return(NULL)

    cores <- setup_cluster(cores)

    by_batch_func <- function(batch) .find_consensus_geno(genotypes[batch,,drop=FALSE])

    batches <- batch_vec(1:nrow(genotypes), n_cores=n_cores(cores))

    result <- unlist(cluster_lapply(cores, batches, by_batch_func))
    result[result=="NA"] <- NA
    names(result) <- rownames(genotypes)

    result
}
