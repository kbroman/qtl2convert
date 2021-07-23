# cross2_ril_to_genail()
#' Convert cross2 object from ril to genril
#'
#' Convert a cross2 object from cross type `"risibn"` to cross type `"genriln"`.
#'
#' @param cross Object of class `"cross2"`, as produced by
#' [qtl2::read_cross2()].
#'
#' @return The input object `cross` with cross type changed to
#' class `"genriln"` for some value of `n`,
#' and the cross information revised to match.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' file <- paste0("https://raw.githubusercontent.com/rqtl/",
#'                "qtl2data/master/CC/cc.zip")
#' cc <- read_cross2(file)
#'
#' cc_genril <- cross2_ril_to_genril(cc)
#' }
#'
#' \dontrun{
#' file <- paste0("https://raw.githubusercontent.com/rqtl/",
#'                "qtl2data/master/ArabMAGIC/arabmagic_tair9.zip")
#' arab <- read_cross2(file)
#'
#' arab_genril <- cross2_ril_to_genril(arab)
#' }

cross2_ril_to_genril <-
    function(cross)
{

    # check that it's cross2
    if(!inherits(cross, "cross2")) stop('Input cross must have class "cross2"')

    # check that it's ril
    crosstype <- cross$crosstype
    valid_types <- c("riself4", "risib4",
                     "riself8", "risib8",
                     "riself16", "magic19", "dh6")

    if(!(crosstype %in% valid_types)) {
        stop('input cross is not one of the allowed types')
    }

    n_str <- as.numeric(gsub("[A-Za-z]+", "", crosstype))
    if(is.na(n_str)) {
        stop("Can't determine the number of strains")
    }

    # crosstype -> genril
    cross$crosstype <- paste0("genril", n_str)

    # equivalent number of generations
    if(grepl("^risib", crosstype)) {
        n_gen <- ceiling((log2(n_str)+4) * 8/7)
    } else { # selfing or just generally
        n_gen <- ceiling((log2(n_str)+1) * 8/7)
    }

    # revise cross_info ->
    new_cross_info <- cbind(n_gen = as.integer(n_gen),
                            matrix(1L, ncol=n_str, nrow=nrow(cross$cross_info)))
    dimnames(new_cross_info) <- list(rownames(cross$cross_info), c("n_gen", LETTERS[seq_len(n_str)]))
    cross$cross_info <- new_cross_info

    cross
}
