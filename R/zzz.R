.onAttach <- function(...) {
  if(requireNamespace("gagglr", quietly = TRUE))
    gagglr::oha_check("glamr", suppress_success = TRUE)
}
