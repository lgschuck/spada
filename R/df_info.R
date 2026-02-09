
#' Info about dataset variables
#'
#' @param df A data.frame object
#'
#' @examples
#' df_info(mtcars)
#'
#' @export
#'

df_info <- function(df) {
  stopifnot(is.data.frame(df))

  if (ncol(df) == 0) {
    return(data.table(
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
    ))
  }

  rows <- fnrow(df)
  cols <- fncol(df)

  res <- lapply(seq_len(cols), function(j) {
    x <- df[[j]]

    nas <- whichNA(x) |> NROW()
    valid <- rows - nas
    uniq <- fnunique(x)
    zeros <- if (is.numeric(x)) NROW(x %==% 0) else 0
    minv <- if (is.numeric(x)) fmin(x, na.rm = T) else NA
    maxv <- if (is.numeric(x)) fmax(x, na.rm = T) else NA

    list(
      var = names(df)[j],
      type = typeof(x),
      class = paste(class(x), collapse = "/"),
      size = as.numeric(object.size(x)),
      min = minv,
      max = maxv,
      n_valid = valid,
      perc_valid = valid / rows,
      n_unique = uniq,
      perc_unique = uniq / rows,
      n_zero = zeros,
      perc_zero = zeros / rows,
      n_nas = nas,
      perc_nas = nas / rows,
      rows = rows,
      cols = cols
    )
  })

  do.call(rbind.data.frame, res) |> as.data.table()
}
