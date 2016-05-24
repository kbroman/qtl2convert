context("find_unique_geno")

test_that("find_unique_geno works", {
    g <- rbind(c(NA,  "A",  "A",  "A", "T"),
               c(NA,  NA,   NA,   "A", "A"),
               c("A", "A",  "T",  "G", "G"),
               rep(NA, 5),
               c("N", "H",  "",   "A", "A"),
               c("N", "H",  "",   "A", "T"),
               c("C", "C",  "G",  "G", NA))
    expected <- rbind(c("A","T"),
                      c(NA, NA),
                      c(NA, NA),
                      c(NA, NA),
                      c(NA, NA),
                      c("A", "T"),
                      c("C", "G"))
    expect_equal(find_unique_geno(g), expected)

    rownames(g) <- rownames(expected) <- paste0("mar", 1:7)
    expect_equal(find_unique_geno(g), expected)

})
