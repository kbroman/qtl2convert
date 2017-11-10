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


test_that("probs convert between DOQTL and R/qtl2", {
    if(isnt_karl()) skip("this test only run locally")

    file <- paste0("https://raw.githubusercontent.com/rqtl/",
                   "qtl2data/master/DOex/DOex.zip")
    DOex <- read_cross2(file)

    # subset to chr 2
    DOex <- DOex[,"2"]

    # calculate genotype probabilities and convert to allele probabilities
    pr <- calc_genoprob(DOex, error_prob=0.002)

    map_tab <- map_list_to_df(DOex$gmap, pos_column="cM")

    # convert to DOQTL format
    pr_doqtl <- probs_qtl2_to_doqtl(pr)

    # convert back
    pr_rqtl2 <- probs_doqtl_to_qtl2(pr_doqtl, map_tab)

    expect_equal(pr_rqtl2, pr)

    # convert back yet again
    pr_doqtl_b <- probs_qtl2_to_doqtl(pr_rqtl2)

    expect_equal(pr_doqtl_b, pr_doqtl)

})
