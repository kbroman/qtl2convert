context("cross2_do_to_genail")

test_that("cross2_do_to_genail works", {
    if(isnt_karl()) skip("this test only run locally")

    file <- paste0("https://raw.githubusercontent.com/rqtl/",
                   "qtl2data/master/DOex/DOex.zip")
    DOex <- read_cross2(file)

    DOex_genail <- cross2_do_to_genail(DOex)

    DOex$crosstype <- "genail8"
    DOex$cross_info <- cbind(DOex$cross_info + 5L,
                             A=1L, B=1L, C=1L, D=1L,
                             E=1L, F=1L, G=1L, H=1L)

    expect_equal(DOex_genail, DOex)

})
