context("write2csv")

test_that("write2csv works", {

    x <- data.frame(a=2:10,
                    b=rnorm(9),
                    c=letters[1:9],
                    stringsAsFactors=FALSE)

    # no row names
    f <- tempfile()
    write2csv(x, f)
    y <- read.csv(f, stringsAsFactors=FALSE, comment.char="#")
    expect_equal(x, y)

    # row names in output
    write2csv(x, f, row.names="id", overwrite=TRUE)
    y <- read.csv(f, stringsAsFactors=FALSE, comment.char="#")
    expect_equal(cbind(id=as.numeric(rownames(x)), x, stringsAsFactors=FALSE), y)

    # row names in input but not output
    rownames(x) <- paste0("V", sample(9))
    write2csv(x, f, overwrite=TRUE)
    y <- read.csv(f, stringsAsFactors=FALSE, comment.char="#")
    rownames(y) <- rownames(x)
    expect_equal(x, y)

    # row names in input and output
    write2csv(x, f, row.names="id", overwrite=TRUE)
    y <- read.csv(f, stringsAsFactors=FALSE, comment.char="#")
    rownames(y) <- y[,1]
    y <- y[,-1]
    expect_equal(x, y)

    # clean up
    unlink(f)

})
