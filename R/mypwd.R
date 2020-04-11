#' Proper credentials from secure file
#'
#' @param username DATIM username
#'
#' @description see keyringr vignette about setting up storing secure credentials
#' @export
#'
#' @examples
#' \dontrun{
#'   myuser <- "UserX"
#'   mypwd(myuser) }

mypwd <- function(username) {

  package_check("keyringr")

  credential_label <- username

  #OS specific options
  if(.Platform$OS.type == "windows"){
    credential_path <- paste0(Sys.getenv("USERPROFILE"),
                              '\\DPAPI\\passwords\\', Sys.info()["nodename"],
                              '\\', credential_label, '.txt')
    pass <- keyringr::decrypt_dpapi_pw(credential_path)
  } else if(.Platform$OS.type == "unix") {
    pass <- keyringr::decrypt_kc_pw(credential_label)
  } else {
    stop("Not a PC or MacOS!")
  }

  return(pass)
}
