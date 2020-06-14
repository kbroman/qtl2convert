# write2csv
#' Write a data frame to a CSV file
#'
#' Write a data frame to a CSV file in a special form, with info about the number of rows and columns.
#'
#' @param df A data frame (or matrix)
#' @param filename File name to write
#' @param comment Comment to place on the first line
#' @param sep Field separator
#' @param comment.char Character to use to initiate the comment lines
#' @param row.names If NA or NULL (the default), row names are not
#' included in the output file. Otherwise, the row names are
#' included as the first column of the output, and this is taken
#' to be the name for that column.
#' @param overwrite If TRUE, overwrite file if it exists. If FALSE
#' (the default) and the file exists, stop with an error.
#'
#' @return None.
#'
#' @details
#' If the file already exists, the function will refuse to write over it.
#'
#' The file will include comments at the top, using `#` as a
#' comment character, including the number of rows (not including the
#' header) and the number of columns.
#'
#' By default, row names are not included. But with the option
#' `row.names` provided as a character string, they are added as an
#' initial column, with the value of this argument defining the name
#' for that column. If a column with that name already exists, the
#' function halts with an error.
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
#'
#' testfile <- file.path(tempdir(), "tmpfile.csv")
#' write2csv(x, testfile, "A file created by write2csv")
#'
#' # Remove the file, to clean up temporary directory
#' unlink(testfile)
write2csv <-
    function(df, filename, comment="", sep=",", comment.char="#",
             row.names=NULL, overwrite=FALSE)
{
    if(!overwrite && file.exists(filename))
        stop(filename, " already exits. Remove it first (or use overwrite=TRUE).")

    if(!is.null(row.names) && !is.na(row.names)) {
        if(length(row.names) > 1) {
            row.names <- row.names[1]
            warning("row.names should have length 1")
        }
        if(!is.character(row.names)) {
            row.names <- as.character(row.names)
            warning("row.names should be a character string")
        }
        if(row.names %in% colnames(df)) {
            stop('There is already a column named "', row.names, '"')
        }
        if(is.null(rownames(df))) {
            rownames(df) <- seq_len(nrow(df))
        }

        df <- cbind(rownames(df), df)
        colnames(df)[1] <- row.names
    }

    if(comment != '')
        cat(comment.char, " ", comment, "\n", file=filename, sep="")

    cat(comment.char, " nrow ", nrow(df), "\n", file=filename, append=(comment!=''),
        sep="")

    cat(comment.char, " ncol ", ncol(df), "\n", file=filename, append=TRUE, sep="")

    cat(paste(colnames(df), collapse=sep), "\n", file=filename, append=TRUE, sep="")

    write.table(df, file=filename, append=TRUE,
                sep=sep, row.names=FALSE, col.names=FALSE, quote=FALSE)
}
