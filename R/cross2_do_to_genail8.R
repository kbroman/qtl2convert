# cross2_do_to_genail8()
#' Convert cross2 object from do to genail8
#'
#' Convert a cross2 object from cross type `"do"` to cross type `"genail8"`.
#'
#' @param cross Object of class `"cross2"`, as produced by
#' [qtl2::read_cross2()].
#'
#' @return The input object `cross` with cross type changed to
#' class `"genail8"` and the cross information revised to match.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' file <- paste0("https://raw.githubusercontent.com/rqtl/",
#'                "qtl2data/master/DOex/DOex.zip")
#' DOex <- read_cross2(file)
#'
#' DOex_genail <- cross2_do_to_genail8(DOex)
#' }

cross2_do_to_genail8 <-
    function(cross)
{

    # check that it's cross2
    if(!inherits(cross, "cross2")) stop('Input cross must have class "cross2"')

    # check that it's DO
    if(!cross$crosstype == "do") stop('input cross is not of type "do"')

    # crosstype -> genail8
    cross$crosstype <- "genail8"

    # revise cross_info ->
    cross$cross_info <- cbind(cross$cross_info+5L,
                                      A=1L, B=1L, C=1L, D=1L, E=1L, F=1L, G=1L, H=1L)

    cross
}


#' @rdname cross2_do_to_genail8
#' @export
cross2_do_to_genail <- cross2_do_to_genail8
