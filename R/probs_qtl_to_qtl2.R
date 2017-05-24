#' Convert R/qtl genotype probabilities to R/qtl2 format
#'
#' Convert R/qtl genotype probabilities to R/qtl2 format
#'
#' @param cross An R/qtl \code{"cross"} object (see
#'     \code{\link[qtl]{read.cross} for details.} Must contain
#'     genotype probabilities as calculated by
#'     \code{\link[qtl]{calc.genoprob}}.
#'
#' @return A list with two components:
#' - \code{"probs"} - the genotype probabilities in the form produced by \code{\link[qtl2geno]{calc_genoprob}}
#' - \code{"map"} - Map of marker/pseudomarker positions (a list of vectors of positions)
#'
#' @examples
#' library(qtl)
#' data(hyper)
#' hyper <- calc.genoprob(hyper, step=1, error.prob=0.002)
#' result <- probs_qtl_to_qtl2(hyper)
#' pr <- result$probs
#' map <- result$map
#'
#' @importFrom stats setNames
#' @export
probs_qtl_to_qtl2 <-
    function(cross)
{
    if(!("cross" %in% class(cross)))
        stop("Input must be an R/qtl cross object")
    if(!("prob" %in% names(cross$geno[[1]])))
        stop("Input doesn't contain genotype probabilities from calc.genoprob")

    # treat bcsft as "f2" but return "bcsft" as the attribute
    crosstype <- class(cross)[1]
    crosstype2 <- crosstype
    if(crosstype=="bcsft") crosstype2 <- "f2"

    chr <- names(cross$geno)

    # chromosome types -> is_x_chr
    chrtype <- sapply(cross$geno, class)
    chrtype[chrtype != "A" & chrtype != "X"] <- "A"
    is_x_chr <- stats::setNames(chrtype=="X", chr)

    # grab probabilities
    pr <- lapply(cross$geno, function(a) a$prob)

    # fix X chr if necessary
    if(any(is_x_chr) && (crosstype2=="bc" || crosstype2=="f2")) {
        sexpgm <- qtl::getsex(cross)
        for(j in seq_along(cross$geno)[is_x_chr])
            pr[[j]] <- revise_x_probs(crosstype2, sexpgm, prob=pr[[j]],
                                      attr(cross, "alleles"))
    }

    # reorder dimensions of the probabilities (ind,pos,genotype) -> (ind,genotype,pos)
    pr <- lapply(pr, function(a) aperm(a, c(1,3,2)))

    # grab map
    map <- lapply(cross$geno, function(a) attr(a$prob, "map"))

    # add chromosome names + individual IDs
    names(pr) <- names(map) <- chr
    ids <- getid(cross)
    if(is.null(ids)) ids <- 1:qtl::nind(cross)
    for(i in seq_along(pr)) rownames(pr[[i]]) <- ids

    attr(pr, "crosstype") <- crosstype
    attr(pr, "is_x_chr") <- is_x_chr
    attr(pr, "alleles") <- attr(cross, "alleles")
    attr(pr, "alleleprobs") <- FALSE
    class(pr) <- c("calc_genoprob", "list")

    list(probs=pr, map=map)
}


# convert the x chromosome probabilities for BC or intercross
revise_x_probs <-
    function(crosstype, sexpgm, prob, alleles)
{
    if(is.null(alleles)) alleles <- c("A", "B")
    gnames <- qtl2geno::geno_names(crosstype, alleles, TRUE)

    result <- array(0, dim=c(dim(prob)[1:2], length(gnames)))
    dimnames(result) <- list(rownames(prob), colnames(prob), gnames)

    if(crosstype=="bc") {
        sex <- sexpgm$sex
        if(is.null(sex)) stop("sex is missing")
        if(any(sex==0)) # females
            result[sex==0,,1:2] <- prob[sex==0,,]
        if(any(sex==1)) # males
            result[sex==1,,3:4] <- prob[sex==1,,]
    }
    else { # "f2"
        if(is.null(sexpgm$sex) || is.null(sexpgm$pgm))
            stop("sex and/or pgm are missing")
        femforw <- (sexpgm$sex==0 & sexpgm$pgm==0)
        femback <- (sexpgm$sex==0 & sexpgm$pgm==1)
        mal <- (sexpgm$sex==1)

        if(any(femforw))
            result[femforw,,1:2] <- prob[femforw,,]
        if(any(femback))
            result[femback,,3:4] <- prob[femback,,2:1]
        if(any(mal))
            result[mal,,5:6] <- prob[back,,]
    }

    result
}
