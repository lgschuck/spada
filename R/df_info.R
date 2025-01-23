
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

  if(ncol(df) == 0){
    data.frame(
      var = 'v1',
      type = NA,
      class = NA,
      size = 0,
      min = NA,
      max = NA,
      n_valid = NA,
      perc_valid = NA,
      n_unique = NA,
      perc_unique = NA,
      n_zero = NA,
      perc_zero = NA,
      n_nas = NA,
      perc_nas = NA,
      rows = NA,
      cols = NA
    )
  } else {
    rows <- nrow(df)
    cols <- ncol(df)
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
      perc_nas = n_nas / rows,
      rows = rep(rows, cols),
      cols = rep(cols, cols)
    )
  }
}
