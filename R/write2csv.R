# write2csv
#' Write a data frame to a CSV file
#'
#' Write a data frame to a CSV file in a special form, with info about the number of rows and columns.
#'
#' @md
#'
#' @param df A data frame (or matrix)
#' @param filename File name to write
#' @param comment Comment to place on the first line
#' @param sep Field separator
#' @param comment.char Character to use to initiate the comment lines
#' @param overwrite If TRUE, overwrite file if it exists. If FALSE
#' (the default) and the file exists, stop with an error.
#'#'
#' @details
#' If the file already exists, the function will refuse to write over it.
#'
#' The file will include comments at the top, using `#` as a
#' comment character, including the number of rows (not including the
#' header) and the number of columns.
#'
#' Row names are not included.
#'
#' @importFrom utils write.table
#' @export
#'
#' @examples
#' nr <- 10
#' nc <- 5
#' x <- data.frame(id=paste0("ind", 1:nr),
#'                 matrix(rnorm(nr*nc), ncol=nc))
#' colnames(x)[1:nc + 1] <- paste0("col", 1:nc)
#' \dontrun{
#' write2csv(x, "/tmp/tmpfile.csv", "A file created by write2csv")}
write2csv <-
    function(df, filename, comment="", sep=",", comment.char="#", overwrite=FALSE)
{
    if(!overwrite && file.exists(filename))
        stop(filename, " already exits. Remove it first (or use overwrite=TRUE).")

    if(comment != '')
        cat(comment.char, " ", comment, "\n", file=filename, sep="")

    cat(comment.char, " nrow ", nrow(df), "\n", file=filename, append=(comment!=''),
        sep="")

    cat(comment.char, " ncol ", ncol(df), "\n", file=filename, append=TRUE, sep="")

    cat(paste(colnames(df), collapse=sep), "\n", file=filename, append=TRUE, sep="")

    write.table(df, file=filename, append=TRUE,
                sep=sep, row.names=FALSE, col.names=FALSE, quote=FALSE)
}
