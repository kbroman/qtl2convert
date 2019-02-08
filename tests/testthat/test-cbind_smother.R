context("cbind_smother")

test_that("cbind_smother works", {

    df1 <- data.frame(x=c(1,2,3,NA,4), y=c(5,8,9,10,11), row.names=c("A", "B", "C", "D", "E"))
    df2 <- data.frame(z=c(7,8,0,9,10), y=c(6,NA,NA,9,10), row.names=c("A", "B", "F", "C", "D"))

    df1n2 <- data.frame(x=c(1,2,3,NA,4,NA), y=c(6,NA,9,10,11,NA), z=c(7,8,9,10,NA,0),
                        row.names=c("A", "B", "C", "D", "E","F"))
    df2n1 <- data.frame(z=c(7,8,0,9,10,NA), y=c(5,8,NA,9,10,11), x=c(1,2,NA,3,NA,4),
                        row.names=c("A", "B", "F", "C", "D", "E"))

    expect_equal(cbind_smother(df1, df2), df1n2)
    expect_equal(cbind_smother(df2, df1), df2n1)

    expect_equal(cbind_smother(df1, df1), df1)
    expect_equal(cbind_smother(df2, df2), df2)

    expect_equal(cbind_smother(df1, df1[-3,]), df1)
    expect_equal(cbind_smother(df2[-2,], df2), df2[c(1,3,4,5,2),])

})
