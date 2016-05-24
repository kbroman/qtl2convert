context("encode_geno")

test_that("encode_geno works", {

    geno <- rbind(c("C", "G", "C",  "GG", "CG"),
                  c("A", "A", "AT", "TA", "TT"),
                  c("T", "G", NA,   "GT", "TT"))
    codes <- rbind(c("C", "G"),
                   c("A", "T"),
                   c("T", "G"))

    expected <- rbind(c("A", "B", "A", "B", "H"),
                      c("A", "A", "H", "H", "B"),
                      c("A", "B", "-", "H", "A"))

    expect_equal(encode_geno(geno, codes), expected)

    expected2 <- gsub("Z", "A",
                      gsub("A", "B",
                           gsub("B", "Z", expected)))

    expect_equal(encode_geno(geno, cbind(codes[,2], codes[,1])), expected2)

})
