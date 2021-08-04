#' Print All Rows
#'
#' `prinf` is a wrapper around `print` that returns all rows rather than
#' just the first 10 by default.
#'
#' @param df data frame
#' @param ... Any other valid option for `print()`.
#'
#' @return prints out all rows rather than default 10 rows.
#' @export
#'
#' @examples
#' \dontrun{
#' df_geo %>% prinf() }

prinf <- function(df, ...) {
  print(df, n = Inf, ...)
}
