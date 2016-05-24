context("find_consensus_geno")

test_that("find_consensus_geno works", {
    g <- rbind(c("NA", "N", "A", "A", "T", "G", NA, "H"),
               c("C",  "C", "G", "G", "A", NA,  NA, NA),
               rep(NA, 8),
               c("C", "C", "G", "G", "G", "C", "G", "G"))
    expected <- c("A", NA, NA, "G")
    expect_equal(find_consensus_geno(g), expected)

    rownames(g) <- names(expected) <- paste0("mar", 1:4)
    expect_equal(find_consensus_geno(g), expected)

})
