
#' Test if value inherit dates
#'
#' @param x values to test
#'
#' @examples
#' Sys.Date() |> is_date()
#'
#' @export
#'
is_date <- function(x){
  inherits(x, c('Date', 'POSIXt', 'POSIXct', 'POSIXlt'))
}
