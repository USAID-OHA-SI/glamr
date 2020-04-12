#' Print All Rpws
#'
#' @param df data frame
#'
#' @return prints out all rows rather than default 10
#' @export
#'
#' @examples
#' \dontrun{
#' df_geo %>% prinf() }

prinf <- function(df) {
  print(df, n = Inf)
}
