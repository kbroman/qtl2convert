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

test_that("find_unique_geno works multi-core", {
    if(isnt_karl()) skip("this test only run locally")

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
    expect_equal(find_unique_geno(g, cores=4), expected)
    expect_equal(find_unique_geno(g, cores=0), expected)

    rownames(g) <- rownames(expected) <- paste0("mar", 1:7)
    expect_equal(find_unique_geno(g), expected)
    expect_equal(find_unique_geno(g, cores=4), expected)
    expect_equal(find_unique_geno(g, cores=0), expected)
})
