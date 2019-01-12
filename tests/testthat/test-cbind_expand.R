context("cbind_expand")

test_that("cbind_expand works", {

    df1 <- data.frame(x=c(1,2,3,NA,4), y=c(5,8,9,10,11), row.names=c("A", "B", "C", "D", "E"))
    df2 <- data.frame(w=c(7,8,0,9,10), z=c(6,NA,NA,9,10), row.names=c("A", "B", "F", "C", "D"))

    df1n2 <- data.frame(x=c(1,2,3,NA,4,NA), y=c(5,8,9,10,11,NA),w=c(7,8,9,10,NA,0),z=c(6,NA,9,10,NA,NA),
                        row.names=c("A", "B", "C", "D", "E","F"))
    df2n1 <- df1n2[c(1,2,6,3:5), c(3:4,1:2)]

    expect_equal(cbind_expand(df1, df2), df1n2)
    expect_equal(cbind_expand(df2, df1), df2n1)

    expect_equal(cbind_expand(df1, df1), cbind(df1, df1))
    expect_equal(cbind_expand(df2, df2), cbind(df2, df2))

})
