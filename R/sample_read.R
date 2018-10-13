#' Read CSV Sample
#'
#' @param path path to filename
#'
#' @return a \code{tibble}
#' @export
#' @importFrom readr read_csv
#' @examples
#' csv = system.file("extdata","school_coordinates.csv",package="BalKmeans")
#' sample_read(csv)
sample_read = function(path){
  readr::read_csv(path)
}
