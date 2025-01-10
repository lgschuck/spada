
#' Test if values are valid names
#'
#' @param x values to test
#'
#' @examples
#' 'abc ' |> is_valid_name()
#' 'abc' |> is_valid_name()
#' c(1, 'a', 'b') |> is_valid_name()
#' list('1', 2, 3, NA, 'a') |> is_valid_name()
#'
#' @export
#'
is_valid_name <- function(x){
  x == make.names(x)
}
