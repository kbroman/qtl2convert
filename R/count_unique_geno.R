# count_unique_geno
#' Count the unique genotypes for each row of a genotype matrix
#'
#' For genotype data (markers x individuals) on a set of individuals,
#' count the unique genotypes for each marker
#'
#' @param genotypes Matrix of genotypes (markers x individuals)
#' @param na.strings Genotypes to be considered as missing values.
#' @param cores Number of CPU cores to use, for parallel calculations.
#' (If `0`, use [parallel::detectCores()].)
#' Alternatively, this can be links to a set of cluster sockets, as
#' produced by [parallel::makeCluster()].
#'
#' @return Vector of counts of unique genotypes.
#'
#' @seealso [find_unique_geno()]
#'
#' @export
#' @importFrom qtl2 batch_vec
#'
#' @examples
#' g <- rbind(c("NA", "A",  "A",  "A", "T"),
#'            c("NA", "NA", "NA", "A", "A"),
#'            c("A",  "A",  "T",  "G", "G"),
#'            c("C", "C",  "G",  "G", "NA"))
#' counts <- count_unique_geno(g)
count_unique_geno <-
function(genotypes, na.strings=c("N", "H", "NA", ""), cores=1)
{
    genotypes[is.na(genotypes) | (genotypes %in% na.strings)]  <- "NA"

    # force to be a matrix
    if(!is.matrix(genotypes)) genotypes <- as.matrix(genotypes)

    # if no rows, return NULL
    if(nrow(genotypes)==0) return(NULL)

    cores <- setup_cluster(cores)

    by_batch_func <- function(batch) .count_unique_geno(genotypes[batch,,drop=FALSE])

    batches <- batch_vec(1:nrow(genotypes), n_cores=n_cores(cores))

    result <- unlist(cluster_lapply(cores, batches, by_batch_func))
    names(result) <- rownames(genotypes)

    result
}
