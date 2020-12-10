#' Load credentials
#'
#'#' @description
#'  `load_secrets` should be set at the beginning of a script to store your
#'  email and DATIM user name under Options for the current session. This allows
#'  analysts to more easily share their scripts without having to manually update
#'  or remove use names.
#'
#'  To initiall store your Google email and DATIM credentials, you will first need to
#'  run `set_email()` and `set_datim()`
#'
#'  `load_secrets` utilizes `keyring` package to access the OS credentials store.
#'  Storing in a centralized, secure location allows analysts to other analysts code
#'  without having to manually change user names/email address to access
#'  DATIM or Google Drive.
#'
#'
#' @return stores Google and DATIM credentials in session
#' @export
#'
#' @importFrom utils installed.packages
#' @importFrom usethis ui_oops
#' @importFrom usethis ui_info
#' @importFrom usethis ui_done
#' @importFrom usethis ui_field
#' @importFrom usethis ui_value
#' @importFrom usethis ui_code
#'
#' @examples
#' \dontrun{
#' load_secrets()
#' user <- getOption("datim")
#' ou_table <- datim_outable(user, datim_pwd()) }

load_secrets <- function(){

  if(length(is_stored()) == 0){
    ui_oops("No accounts stored under {ui_code('keyring')}. Use {ui_code('set_email()')} and {ui_code('set_datim()')} to establish accounts")
  } else {
    ui_info("The following items have been stored for use in this session:")
  }

  if(is_stored("email")){
    options("email" = keyring::key_list("email")[1,2])
    ui_done("{ui_field('email')} set as {ui_value(getOption('email'))}")
  }

  if(is_stored("email") && is_installed("googledrive")){
    options(googledrive::drive_auth(getOption("email")))
    ui_done("{ui_code('googledrive')} authenticated using {ui_field('email')}")
  }

  if(is_stored("email") && is_installed("googlesheets4")){
    options(googlesheets4::gs4_auth(getOption("email")))
    ui_done("{ui_code('googlesheets4')} authenticated using {ui_field('email')}")
  }

  if(is_stored("datim")){
    options("datim" = keyring::key_list("datim")[1,2])
    options("baseurl" = "https://final.datim.org/")
    ui_done("{ui_field('datim')} username set as {ui_value(getOption('datim'))}")
    ui_done("{ui_field('baseurl')} set to {ui_value(getOption('baseurl'))}")
  }

}



#' Set USAID email
#'
#' @description
#' `set_email` stores your USAID email using the `keyring` package. This will
#' only need to run once.
#'
#'  The `keyring` package utilized the OS credentials store. Storing in
#'  a centralized, secure location allows analysts to other analysts code
#'  without having to manually change user names/email address to access
#'  DATIM or Google Drive.
#'
#'  After `set_email` has been run once, an analyst can set `load_secrets` at the
#'  beginning of a script, storing their email and DATIM username under Options
#'  for the current session.
#'
#' @param usaid_email full USAID email address
#'
#' @return stores USAID email in using keyring
#' @export
#'
#' @examples
#' \dontrun{
#' set_email("rshah@usaid.gov") }
#'
set_email <- function(usaid_email){
  keyring::key_set_with_value(service = "email",
                              username = usaid_email,
                              password = "NULL")
}

#' Store DATIM credentials
#'
#' @description
#' `set_datim` stores your DATIM credentials email using the `keyring` package.
#' This will only need to done once. After running `set_datim(user)`, you will be
#' promoted to enter your password through the RStudio API which will then store the
#' username and password in your OS credential store using `keyring`.
#'
#'  The `keyring` package utilized the OS credentials store. Storing in
#'  a centralized, secure location allows analysts to other analysts code
#'  without having to manually change user names/email address to access
#'  DATIM or Google Drive.
#'
#'  After `set_datim` has been run once, an analyst can set `load_secrets` at the
#'  beginning of a script, storing their email and DATIM username under Options
#'  for the current session.
#'
#' @param datim_username DATIM account
#'
#' @return stores USAID email in using keyring
#' @export
#'
#' @examples
#' \dontrun{
#' set_datim("rshah") }
#'
set_datim <- function(datim_username){
  keyring::key_set(service = "datim",
                   username = datim_username)
}

#' Return DATIM username
#'
#' @return access DATIM username from keyring
#' @export
#'
#' @importFrom usethis ui_stop
#' @importFrom usethis ui_code
#' @examples
#' \dontrun{
#' load_secrets()
#' ou_table <- datim_outable(datim_user(), datim_pwd()) }

datim_user <- function(){

  if(!is_stored("datim"))
    ui_stop("NO DATIM credentials stored. Setup using {ui_code('set_datim()')}")

  if(!is.loaded("datim"))
    suppressMessages(load_secrets())

  keyring::key_list("datim")[1,2]

}

#' Return DATIM password
#'
#' @return access DATIM password from keyring
#' @export
#'
#' @importFrom usethis ui_stop
#' @importFrom usethis ui_code
#'
#' @examples
#' \dontrun{
#' load_secrets()
#' ou_table <- datim_outable(datim_user(), datim_pwd()) }

datim_pwd <- function(){

  if(!is_stored("datim"))
    ui_stop("NO DATIM credentials stored. Setup using {:ui_code('set_datim()')}")

  if(!is.loaded("datim"))
    suppressMessages(load_secrets())

  keyring::key_get("datim", getOption("datim"))

}
