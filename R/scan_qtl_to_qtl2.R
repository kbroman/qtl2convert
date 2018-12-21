#' Convert R/qtl scanone results to R/qtl2 scan1 format
#'
#' Convert the results of R/qtl1 [qtl::scanone()] to the form used by the R/qtl2 [qtl2::scan1()].
#'
#' @md
#'
#' @param scanone_output Data frame as output by the R/qtl1 function [qtl::scanone()].
#'
#' @return List with two objects: the LOD scores in [qtl2::scan1()]
#'     format, and the map (as a list of marker/pseudomarker
#'     positions).
#'
#' @examples
#' library(qtl)
#' data(hyper)
#' hyper <- calc.genoprob(hyper, step=1, error.prob=0.002)
#' out <- scanone(hyper)
#' out2 <- scan_qtl_to_qtl2(out)
#'
#' library(qtl2)
#' plot(out2$scan1, out2$map)
#'
#' @seealso [scan_qtl_to_qtl2()]
#'
#' @export
scan_qtl_to_qtl2 <-
    function(scanone_output)
{
  scan1 <- as.matrix(scanone_output[,-(1:2),drop=FALSE])
  class(scan1) <- c("scan1", "matrix")

  map_df <- cbind(as.data.frame(scanone_output[,1:2]), marker=rownames(scanone_output))
  map_list <- map_df_to_list(map_df, pos_column="pos")

  list(scan1=scan1, map=map_list)
}
