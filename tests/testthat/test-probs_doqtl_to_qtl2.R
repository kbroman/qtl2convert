context("Convert genoprobs from DOQTL to qtl2")

test_that("probs_doqtl_to_qtl2 works", {

    n_ind <- 20
    n_gen <- 8
    n_pos <- 10
    d <- c(n_ind, n_gen, n_pos)
    p <- array(runif(prod(d)), dim=d)
    dimnames(p) <- list(paste0("ind", 1:n_ind),
                        LETTERS[1:8],
                        paste0("marker", 1:n_pos))


    map <- data.frame(marker=paste0("marker", 1:n_pos),
                      chr=rep(c("1", "2"), each=n_pos/2),
                      Mbp=c(sort(runif(5, 0, 100)), sort(runif(5, 0, 50))),
                      stringsAsFactors=FALSE)

    expect_error( probs_doqtl_to_qtl2(p, map) ) # no pos column
    result <- probs_doqtl_to_qtl2(p, map, pos_column="Mbp")

    expected <- list("1"=p[,,1:5], "2"=p[,,6:10])
    attr(expected, "is_x_chr") <- c("1"=FALSE,"2"=FALSE)
    attr(expected, "crosstype") <- "do"
    attr(expected, "alleles") <- LETTERS[1:8]
    attr(expected, "alleleprobs") <- TRUE
    class(expected) <- c("calc_genoprob", "list")

    expect_equal( result, expected )

})
