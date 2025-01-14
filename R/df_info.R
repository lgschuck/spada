
#' Info about dataset variables
#'
#' @param df A data.frame object
#'
#' @examples
#' df_info(mtcars)
#'
#' @export
#'

df_info <- function(df){

  stopifnot(is.data.frame(df))
  rows <- nrow(df)
  n_nas <- sapply(df, \(x) if(anyNA(x)) suna(is.na(x)) else 0L)
  n_valid <- rows - n_nas
  n_unique <- sapply(df, \(x) length(unique(x)))
  n_zero <- sapply(df, \(x) suna(x == 0))

  data.frame(
    var = names(df),
    type = sapply(df, typeof),
    class = sapply(df, \(x) paste(class(x), collapse = "/")),
    size = sapply(df, object.size),
    min = sapply(df, \(x) if (is.numeric(x)) mina(x) else NA),
    max = sapply(df, \(x) if (is.numeric(x)) mana(x) else NA),
    n_valid = n_valid,
    perc_valid = n_valid / rows,
    n_unique = n_unique,
    perc_unique = n_unique / rows,
    n_zero = n_zero,
    perc_zero = n_zero / rows,
    n_nas = n_nas,
    perc_nas = n_nas / rows
  )
}
