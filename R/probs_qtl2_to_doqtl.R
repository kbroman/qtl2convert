#' Convert R/qtl2 genotype probabilities to DOQTL format
#'
#' Convert R/qtl2 genotype probabilities to DOQTL format
#'
#' @param probs A `"calc_genoprob"` object (a list of 3d arrays of
#' genotype probabilities), as calculated by [qtl2::calc_genoprob()].
#'
#' @return A single three-dimensional array, for use with
#'     [DOQTL](https://github.com/dmgatti/DOQTL).
#'
#' @details
#' If the arrays in `probs` all have 8 columns, they're assumed to be
#' allele dosages and we paste them all together into one big array.
#'
#' Otherwise, it should be that the autosomes all have 36 columns the
#' X chromosome has 44. In this case, the male hemizygotes on the X
#' are placed where the female homozygotes are, and then we reorder
#' the genotypes into alphabetical order.
#'
#' @export
probs_qtl2_to_doqtl <-
    function(probs)
{
    is_x_chr <- attr(probs, "is_x_chr")
    n_geno <- sapply(probs, ncol)

    crosstype <- attr(probs, "crosstype")
    if(is.null(crosstype))
        warning('Expecting crosstype "do" but it is missing.')
    else if(crosstype!="do")
        warning('Expecting crosstype "do" but it is "', crosstype, '"')

    n_pos <- sapply(probs, function(a) dim(a)[3])

    if(all(n_geno==8)) { # allele dosages

        # big array to contain the results
        result <- array(dim=c(nrow(probs[[1]]), ncol(probs[[1]]), sum(n_pos)))
        dimnames(result) <- list(rownames(probs[[1]]), colnames(probs[[1]]),
                                 unlist(lapply(probs, function(a) dimnames(a)[[3]])))

        # copy over one chromosome at a time
        cur <- 0
        for(i in seq_along(probs)) {
            result[,,cur + 1:n_pos[i]] <- probs[[i]]
            cur <- cur + n_pos[i]
        }

        return(result)
    }
    else { # genotype probabilities
        if(!all(n_geno[is_x_chr] == 44) || !all(n_geno[!is_x_chr]==36))
            stop("There should be 44 genotypes on the X and 36 on the autosomes.")

        # get allele labels
        alleles <- attr(probs, "alleles")
        if(is.null(alleles)) alleles <- LETTERS[1:8]
        if(length(alleles) != 8)
            stop("length(alleles) != 8")

        # deal with X chromosome (paste male hemizygotes where the female homozygotes are)
        homozyg <- paste0(alleles, alleles)
        hemizyg <- paste0(alleles, "Y")
        for(chr in names(probs)[is_x_chr]) { # X chr
            probs[[chr]][,homozyg,] <- probs[[chr]][,homozyg,,drop=FALSE] + probs[[chr]][,hemizyg,,drop=FALSE]
            probs[[chr]] <- probs[[chr]][,1:36,,drop=FALSE]
        }

        # new order of genotype labels
        mat <- rbind(rep(1:8, each=8), rep(1:8, 8))
        geno_labels <- apply(mat[,mat[1,] <= mat[2,]], 2, function(a) paste(alleles[a], collapse=""))

        # big array to contain the results
        result <- array(dim=c(nrow(probs[[1]]), ncol(probs[[1]]), sum(n_pos)))
        dimnames(result) <- list(rownames(probs[[1]]), geno_labels,
                                 unlist(lapply(probs, function(a) dimnames(a)[[3]])))

        # copy over one chromosome at a time, also re-ordering the genotypes
        cur <- 0
        for(i in seq_along(probs)) {
            result[,,cur + 1:n_pos[i]] <- probs[[i]][,geno_labels,,drop=FALSE]
            cur <- cur + n_pos[i]
        }

        return(result)

    }

}
