context("convert qtl2scan::scan1 to qtl::scanone output")

test_that("scan_qtl2_to_qtl works", {

    library(qtl)
    data(hyper)
    hyper <- calc.genoprob(hyper[c(6,8),], step=2.5, err=0.01)
    out_scanone <- scanone(hyper, method="hk")

    # pull out pseudomarker map and adjust names
    # (want to use the exact same map)
    pmap <- lapply(hyper$geno, function(a) {
        m <- attr(a$prob, "map")
        m[grep("^loc", names(m))] })
    for(i in seq(along=pmap))
        names(pmap[[i]]) <- paste0("c", names(pmap)[i], ".",
                                   names(pmap[[i]]))

    library(qtl2geno)
    hyper2 <- convert2cross2(hyper)
    map <- insert_pseudomarkers(pull.map(hyper), pseudomarker_map=pmap)

    pr <- calc_genoprob(hyper2, map, err=0.01)
    library(qtl2scan)
    out_scan1 <- scan1(pr, hyper$pheno[,1,drop=FALSE])

    # adjustments to scanone output
    colnames(out_scanone)[3] <- "bp"
    for(atname in c("method", "type", "model"))
        attr(out_scanone, atname) <- NULL

    out_conv <- scan_qtl2_to_qtl(out_scan1, map)
    expect_equal(out_conv, out_scanone)

})
