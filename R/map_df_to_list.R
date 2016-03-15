#' Marker map data frame to list
#'
#' Convert a marker map organized as data frame to a list
#'
#' @param map Data frame with marker map
#'
#' @param chr_column Name of the column in \code{map} that contains the chromosome IDs.
#'
#' @param pos_column Name of the column in \code{map} that contains the marker positions.
#'
#' @param marker_column Name of the column in \code{map} that contains
#' the marker names. If NULL, use the row names.
#'
#' @return A list of vectors of marker positions, one component per chromosome
#'
#' @export
map_df_to_list <-
    function(map, chr_column="chr", pos_column="cM", marker_column="marker")
{
    if(is.null(marker_column)) {
        marker_column <- "qtl2tmp_marker"
        map[,marker_column] <- rownames(marker_column)
    }
    if(!(marker_column %in% colnames(map)))
        stop('Column "', marker_column, '" not found.')
    if(!(chr_column %in% colnames(map)))
        stop('Column "', chr_column, '" not found.')
    if(!(pos_column %in% colnames(map)))
        stop('Column "', pos_column, '" not found.')

    marker <- map[,marker_column]

    chr <- map[,chr_column]
    uchr <- unique(chr)
    pos <- map[,pos_column]

    result <- split(pos, factor(chr, levels=uchr))
    marker <- split(marker, factor(chr, levels=uchr))
    for(i in seq(along=result))
        names(result[[i]]) <- marker[[i]]

    result
}
