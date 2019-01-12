#' Combine matrices by columns, replacing matching ones and adding unique ones
#'
#' This is like [base::cbind()] but if a column in the second matrix
#' has the same name as a column in the first matrix, the column in
#' the first matrix is deleted and that in the second matrix is used
#' in its place.
#'
#' @md
#'
#' @param mat1 A matrix
#' @param mat2 Another matrix, with the same number of rows as `mat`.
#'
#' @return The two matrices combined by columns, but columns in the
#' first matrix that also appear in the second matrix are deleted
#' and replaced by those in the second matrix. Uses the row names
#' to align the rows in the two matrices, and to expand them as needed.
#'
#' @export
#'
#' @examples
#' df1 <- data.frame(x=c(1,2,3,NA,4), y=c(5,8,9,10,11), row.names=c("A", "B", "C", "D", "E"))
#' df2 <- data.frame(z=c(7,8,0,9,10), y=c(6,NA,NA,9,10), row.names=c("A", "B", "F", "C", "D"))
#' cbind_smother(df1, df2)
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

        mat1 <- cbind_expand(mat1, mat2[,!m_col,drop=FALSE])
        mat1[rn2, m_col] <- mat2[rn2, m_col]
    }
    else {
        mat1 <- cbind_expand(mat1, mat2)
    }

    mat1
}


# cbind, matching rownames and expanding with NAs as needed
#
# does cbind(mat1, mat2)
# but: - looks at rownames and makes sure they line up
#      - if rows are in one matrix but not the other,
#        creates row of NAs where missing
#
# copied from qtl2, where it is an internal function
cbind_expand <-
    function(...)
{
    input <- list(...)

    if(length(input)<2) return(input[[1]])

    # check IDs
    id <- lapply(input, rownames)
    if(any(vapply(id, is.null, TRUE)))
        stop("All input matrices must have rownames (containing individual IDs)")
    if(any(vapply(id, function(a) length(unique(a)) != length(a), TRUE)))
        stop("Some input matrices have duplicate rownames")
    uid <- unique(unlist(id))

    for(i in seq(along=input)) {
        not_in <- !(uid %in% id[[i]])
        if(!any(not_in)) next
        missing <- uid[not_in]

        # create new rows
        new_rows <- matrix(NA, ncol=ncol(input[[i]]), nrow=length(missing))
        dimnames(new_rows) <- list(missing, colnames(input[[i]]))

        input[[i]] <- rbind(input[[i]], new_rows)
    }

    for(i in seq(along=input)) {
        if(i==1)
            input[[1]] <- input[[1]][uid,,drop=FALSE]
        else
            input[[1]] <- cbind(input[[1]], input[[i]][uid,,drop=FALSE])
    }

    input[[1]]
}
