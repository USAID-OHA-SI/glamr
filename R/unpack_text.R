#' Extract text
#' @param txt text containing parenthesis
#' @param limits area to extract text from, c("()", "{}", "[]")
#' @return text within limits
#' @export
#' \dontrun{
#'  extract_text(txt = "Saint Mary Hopital (SMH)")
#'  extract_text(txt = "TDB [Placeholder - New Mechnism]") }

extract_text <- function(txt, limits = "()") {

  # Validate limi
  options <- c("()", "[]", "{}")

  if (!limits %in% options) {
    cat("\nERROR - invalid limits option: ",
        Wavelength::paint_red({{limits}}), "\n")

    return(NULL)
  }

  # filter limits
  if ( limits == "()" ) {
    limits <- "\\(.*\\)"
    content <- "(?<=\\().*(?=\\))"
  }
  else if ( limits == "[]" ) {
    limits <- "\\[.*\\]"
    content <- "(?<=\\[).*(?=\\])"
  }
  else if ( limits == "{}" ) {
    limits <- "\\{.*\\}"
    content <- "(?<=\\{).*(?=\\})"
  }

  # Extract text with limits
  txt <- dplyr::case_when(
    stringr::str_detect(txt, limits) ~ stringr::str_extract(txt, content),
    TRUE ~ txt
  )

  return(txt)
}
