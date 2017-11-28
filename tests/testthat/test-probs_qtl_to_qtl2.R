context("probs_qtl_to_qtl2")

test_that("probs_qtl_to_qtl2 works for backcross", {

    library(qtl)
    library(qtl2)

    data(hyper)
    hyper <- hyper[c(1,6,"X"),]
    hyper <- calc.genoprob(hyper, step=1, error.prob=0.002)

    hyper2 <- qtl2::convert2cross2(hyper)
    m <- lapply(hyper$geno, function(a) attr(a$prob, "map"))
    pr <- calc_genoprob(hyper2, m, error_prob=0.002)

    result <- probs_qtl_to_qtl2(hyper)
    expect_equal(result$map, m)
    expect_equal(result$probs, pr)

})


test_that("probs_qtl_to_qtl2 works for intercross", {

    set.seed(92493498)

    library(qtl)
    library(qtl2)

    data(listeria)
    listeria <- listeria[c(1,8,"X"),]
    listeria <- calc.genoprob(listeria, step=1, error.prob=0.002)

    listeria2 <- qtl2::convert2cross2(listeria)
    m <- lapply(listeria$geno, function(a) attr(a$prob, "map"))
    pr <- calc_genoprob(listeria2, m, error_prob=0.002)

    result <- probs_qtl_to_qtl2(listeria)
    expect_equal(result$map, m)
    expect_equal(result$probs, pr)

    # all males
    listeria$pheno$sex <- "male"
    listeria2 <- qtl2::convert2cross2(listeria)
    listeria <- calc.genoprob(listeria, step=1, error.prob=0.002)
    m <- lapply(listeria$geno, function(a) attr(a$prob, "map"))
    pr <- calc_genoprob(listeria2, m, error_prob=0.002)
    result <- probs_qtl_to_qtl2(listeria)
    expect_equal(result$map, m)
    expect_equal(result$probs, pr)

    # all females, backward direction
    listeria$pheno$sex <- "female"
    listeria$pheno$pgm <- 1
    listeria2 <- qtl2::convert2cross2(listeria)
    listeria <- calc.genoprob(listeria, step=1, error.prob=0.002)
    m <- lapply(listeria$geno, function(a) attr(a$prob, "map"))
    pr <- calc_genoprob(listeria2, m, error_prob=0.002)
    result <- probs_qtl_to_qtl2(listeria)
    expect_equal(result$map, m)
    expect_equal(result$probs, pr)

    # all females, both directions
    listeria$pheno$pgm <- sample(0:1, nind(listeria), replace=TRUE)
    listeria2 <- qtl2::convert2cross2(listeria)
    listeria <- calc.genoprob(listeria, step=1, error.prob=0.002)
    m <- lapply(listeria$geno, function(a) attr(a$prob, "map"))
    pr <- calc_genoprob(listeria2, m, error_prob=0.002)
    result <- probs_qtl_to_qtl2(listeria)
    expect_equal(result$map, m)
    expect_equal(result$probs, pr)

    # males and females, forward
    listeria$pheno$sex <- sample(c("female", "male"), nind(listeria), replace=TRUE)
    listeria$pheno$pgm <- 0
    listeria2 <- qtl2::convert2cross2(listeria)
    listeria <- calc.genoprob(listeria, step=1, error.prob=0.002)
    m <- lapply(listeria$geno, function(a) attr(a$prob, "map"))
    pr <- calc_genoprob(listeria2, m, error_prob=0.002)
    result <- probs_qtl_to_qtl2(listeria)
    expect_equal(result$map, m)
    expect_equal(result$probs, pr)

    # males and females, backward
    listeria$pheno$sex <- sample(c("female", "male"), nind(listeria), replace=TRUE)
    listeria$pheno$pgm <- 1
    listeria2 <- qtl2::convert2cross2(listeria)
    listeria <- calc.genoprob(listeria, step=1, error.prob=0.002)
    m <- lapply(listeria$geno, function(a) attr(a$prob, "map"))
    pr <- calc_genoprob(listeria2, m, error_prob=0.002)
    result <- probs_qtl_to_qtl2(listeria)
    expect_equal(result$map, m)
    expect_equal(result$probs, pr)

    # both sex, both directions
    listeria$pheno$sex <- sample(c("female", "male"), nind(listeria), replace=TRUE)
    listeria$pheno$pgm <- sample(0:1, nind(listeria), replace=TRUE)
    listeria2 <- qtl2::convert2cross2(listeria)
    listeria <- calc.genoprob(listeria, step=1, error.prob=0.002)
    m <- lapply(listeria$geno, function(a) attr(a$prob, "map"))
    pr <- calc_genoprob(listeria2, m, error_prob=0.002)
    result <- probs_qtl_to_qtl2(listeria)
    expect_equal(result$map, m)
    expect_equal(result$probs, pr)

})
