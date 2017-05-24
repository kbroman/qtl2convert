context("probs_qtl_to_qtl2")

test_that("probs_qtl_to_qtl2 works for backcross", {

    library(qtl)
    data(hyper)
    hyper <- hyper[c(1,6,"X"),]
    hyper <- calc.genoprob(hyper, step=1, error.prob=0.002)

    hyper2 <- convert2cross2(hyper)
    m <- lapply(hyper$geno, function(a) attr(a$prob, "map"))
    pr <- calc_genoprob(hyper2, m, error_prob=0.002)

    result <- probs_qtl_to_qtl2(hyper)
    expect_equal(result$map, m)
    expect_equal(result$probs, pr)

})
