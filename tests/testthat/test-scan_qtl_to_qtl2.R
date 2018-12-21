context("convert qtl2::scanone to qtl2::scan1 output")

test_that("scan_qtl_to_qtl2 works", {

    library(qtl)
    data(hyper)
    hyper <- calc.genoprob(hyper[c(6,8),], step=2.5, err=0.01)
    out_scanone <- scanone(hyper, method="hk")
    out_scan1_conv <- scan_qtl_to_qtl2(out_scanone)

    # pull out pseudomarker map and adjust names
    # (want to use the exact same map)
    pmap <- lapply(hyper$geno, function(a) {
        m <- attr(a$prob, "map")
        m[grep("^loc", names(m))] })
    for(i in seq(along=pmap))
        names(pmap[[i]]) <- paste0("c", names(pmap)[i], ".",
                                   names(pmap[[i]]))

    library(qtl2)
    hyper2 <- convert2cross2(hyper)
    map <- insert_pseudomarkers(pull.map(hyper), pseudomarker_map=pmap)

    pr <- calc_genoprob(hyper2, map, err=0.01)
    out_scan1 <- scan1(pr, hyper$pheno[,1,drop=FALSE])

    expect_equivalent(out_scan1, out_scan1_conv$scan1)
    expect_equivalent(map, out_scan1_conv$map)

})
