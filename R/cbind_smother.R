#' Combine matrices by columns, replacing matching ones and adding unique ones
#'
#' This is like [base::cbind()] but if a column in the second matrix
#' has the same name as a column in the first matrix, the column in
#' the first matrix is deleted and that in the second matrix is used
#' in its place.
#'
#' @param mat1 A matrix
#' @param mat2 Another matrix, with the same number of rows as `mat`.
#'
#' @return The two matrices combined by columns, but columns in the
#' first matrix that also appear in the second matrix are deleted
#' and replaced by those in the second matrix. Uses the row names
#' to align the rows in the two matrices, and to expand them as needed.
#'
#' @importFrom qtl2 cbind_expand
#' @export
#'
#' @examples
#' df1 <- data.frame(x=c(1,2,3,NA,4), y=c(5,8,9,10,11), row.names=c("A", "B", "C", "D", "E"))
#' df2 <- data.frame(z=c(7,8,0,9,10), y=c(6,NA,NA,9,10), row.names=c("A", "B", "F", "C", "D"))
#' df1n2 <- cbind_smother(df1, df2)
cbind_smother <-
  function(mat1, mat2)
{
    cn1 <- colnames(mat1)
    cn2 <- colnames(mat2)
    if(is.null(cn1) || is.null(cn2)) {
        stop("Need colnames for both mat1 and mat2")
    }

    m_col <- (cn2 %in% cn1)

    if(any(m_col)) {
        rn1 <- rownames(mat1)
        rn2 <- rownames(mat2)
        if(is.null(rn1) || is.null(rn2)) {
            stop("Need rownames for both mat1 and mat2")
        }
        m_row <- match(rn1[rn1 %in% rn2], rn2)

        mat1 <- qtl2::cbind_expand(mat1, mat2[,!m_col,drop=FALSE])
        mat1[rn2, m_col] <- mat2[rn2, m_col]
    }
    else {
        mat1 <- qtl2::cbind_expand(mat1, mat2)
    }

    mat1
}
