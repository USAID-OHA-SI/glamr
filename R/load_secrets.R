#' Load credentials
#'
#'  `load_secrets` should be set at the beginning of a script to store your
#'  email and DATIM user name under Options for the current session. This allows
#'  analysts to more easily share their scripts without having to manually update
#'  or remove use names.
#'
#'  To initiall store your Google email and DATIM credentials,
#'  you will first need to
#'  run `set_email()`, `set_datim()`, `set_s3access()` and `set_s3secret()`
#'
#'  `load_secrets` utilizes `keyring` package to access the OS credentials store.
#'  Storing in a centralized, secure location allows analysts to other analysts code
#'  without having to manually change user names/email address to access
#'  DATIM or Google Drive.
#'
#'
#' @param service account, either "email", "datim" or "s3"; by default, all are
#' loaded if they are available
#'
#' @return stores Google, DATIM, and s3 credentials in session
#' @export
#' @family authentication
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

load_secrets <- function(service = c("email", "datim", "s3")){

  if(length(is_stored()) == 0){
    ui_oops("No accounts stored under {ui_code('keyring')}. Use {ui_code('set_email()')} and {ui_code('set_datim()')} to establish accounts")
  } else {
    ui_info("The following items have been stored for use in this session:")
  }

  if(is_stored("email") && "email" %in% service){
    options("email" = keyring::key_list("email")[1,2])
    ui_done("{ui_field('email')} set as {ui_value(getOption('email'))}")
  }

  if(is_stored("email") && is_installed("googledrive") && "email" %in% service){
    options(googledrive::drive_auth(getOption("email")))
    ui_done("{ui_code('googledrive')} authenticated using {ui_field('email')}")
  }

  if(is_stored("email") && is_installed("googlesheets4") && "email" %in% service){
    options(googlesheets4::gs4_auth(getOption("email")))
    ui_done("{ui_code('googlesheets4')} authenticated using {ui_field('email')}")
  }

  if(is_stored("datim") && "datim" %in% service){
    options("datim" = keyring::key_list("datim")[1,2])
    options("baseurl" = "https://final.datim.org/")
    ui_done("{ui_field('datim')} username set as {ui_value(getOption('datim'))}")
    ui_done("{ui_field('baseurl')} set to {ui_value(getOption('baseurl'))}")
  }

  if (is_stored("s3") && "s3" %in% service) {
    options("access_key" = get_s3key("access"))
    options("secret_key" = get_s3key("secret"))

    # Note: aws.s3 setting uses the below env settings
    # It's up to the user to decide when to set this (in the GLOBALS Sections)
    # Sys.setenv(
    #   "AWS_ACCESS_KEY_ID" = get_s3key("access"),
    #   "AWS_SECRET_ACCESS_KEY" = get_s3key("secret"),
    #   "AWS_REGION" = "us-east-1"
    # )

    ui_done("{ui_field('S3')} keys set in {ui_value('access_key')} and {ui_value('secret_key')}")
  }

}



#' Store USAID email
#'
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
#'  This function also stores the email locally in your .Rprofile, allowing to be
#'  used automatically as the default for `googledrive::drive_auth()` and
#'  `googlesheets4::gs4_auth()`
#'
#' @param usaid_email full USAID email address
#'
#' @return stores USAID email using keyring and .Rprofile
#' @export
#' @family authentication
#'
#' @importFrom usethis ui_path
#' @importFrom usethis ui_todo
#' @importFrom usethis ui_code_block
#'
#' @examples
#' \dontrun{
#' set_email("rshah@usaid.gov") }
#'
set_email <- function(usaid_email){
  keyring::key_set_with_value(service = "email",
                              username = usaid_email,
                              password = "NULL")
  ui_code_block(
    "
    options(gargle_oauth_email = '{usaid_email}')
    "
  )

  ui_todo("Copy and paste this code in {ui_path('.Rprofile')} to store your SI folder paths.")

  usethis::edit_r_profile("user")
}

#' Store DATIM credentials
#'
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
#' @family authentication
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
#' To setup/store, run `glamr::set_datim()`.
#'
#' @return access DATIM username from keyring
#' @export
#' @family authentication
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
#' To setup/store, run `glamr::set_datim()`.
#'
#' @return access DATIM password from keyring
#' @export
#' @family authentication
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

#' @title Store S3 Credentials
#'
#' `set_s3keys` stores your s3 keys using the `keyring` package.
#' This will only need to done once. After running `set_s3keys(access, secret)`,
#' RStudio API which will then store the keys in your OS credential store using `keyring`.
#'
#' @param access S3 Account Access Key
#' @param secret S3 Account Secret Key
#'
#' @return stored access key
#' @export
#' @family authentication
#'
#' @examples
#' \dontrun{
#' set_s3access("ABDCEDFF", "MLIZD998SD") }
#'
set_s3keys <- function(access, secret){
  keyring::key_set_with_value(service = "s3",
                              username = "access",
                              password = access)

  keyring::key_set_with_value(service = "s3",
                              username = "secret",
                              password = secret)
}


#' Get S3 Credentials - Access or Secret Access Key
#'
#' `get_s3key` retrieves your S3 keys using the `keyring` package.
#' Set name to `access` for `Access Key`,
#' `name` to `secret` for `Secret Access Key`
#'
#' @param name S3 account key
#'
#' @return stored key
#' @export
#' @family authentication
#'
#' @examples
#' \dontrun{
#' get_s3key(name = "access") }
#'
get_s3key <- function(name = "access"){
  keyring::key_get(service = "s3", username = name)
}


