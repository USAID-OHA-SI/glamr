#' Load credentials
#'
#'  `load_secrets` should be set at the beginning of a script to store your
#'  email and DATIM user name under Options for the current session. This allows
#'  analysts to more easily share their scripts without having to manually update
#'  or remove use names.
#'
#'  To initially store your credentials, you will first need to
#'  run `set_email()`, `set_datim()`, `set_pano()`, and/or `set_key` (for s3)
#'
#'  `load_secrets` utilizes `keyring` package to access the OS credentials store.
#'  Storing in a centralized, secure location allows analysts to other analysts code
#'  without having to manually change user names/email address to access
#'  DATIM or Google Drive.
#'
#'
#' @param service account, either "email", "datim", "pano", "s3", or "pdap"; by
#' default, all are loaded if they are available
#'
#' @return stores Google, DATIM, PEFPFAR Panorama, s3, and PDAP credentials in
#'   session
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
#' @importFrom usethis ui_line
#'
#' @examples
#' \dontrun{
#' load_secrets()
#' ou_table <- datim_outable(datim_user(), datim_pwd()) }

load_secrets <- function(service = c("email", "datim", "pano","s3", "pdap")){

  package_check('keyring')

  if(length(is_stored()) == 0){
    ui_oops("No accounts stored under {ui_code('keyring')}. Use {ui_code('set_email()')}, {ui_code('set_datim()')}, or {ui_code('set_keys()')} to establish accounts")
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

  if(is_stored("pano") && "pano" %in% service){
    options("pano" = keyring::key_list("pano")[1,2])
    ui_done("{ui_field('pano')} username set as {ui_value(getOption('pano'))}")
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

  if (is_stored("pdap") && "pdap" %in% service) {
    options("pdap_access" = pdap_access())
    options("pdap_secret" = pdap_secret())
    options("pdap_read" = pdap_bucket("read"))
    options("pdap_write" = pdap_bucket("write"))
    ui_done("{ui_field('PDAP S3')} keys set in {ui_value('pdap_access()')} and {ui_value('pdap_key()')}")
    if(is_pdap()){
      ui_line("Also accessible through {ui_code('Sys.getenv(\\'AWS_ACCESS_KEY_ID\\')')} and {ui_code('Sys.getenv(\\'AWS_SECRET_ACCESS_KEY\\')')}")
      ui_done("{ui_field('PDAP S3')} READ/WRITE buckets available through {ui_value('pdap_read()')} and {ui_value('pdap_write()')}")
      ui_line("Also accessible through {ui_code('Sys.getenv(\\'S3_READ\\')')} and {ui_code('Sys.getenv(\\'S3_WRITE\\')')}")
    } else {
      ui_done("{ui_field('PDAP S3')} WRITE bucket available through {ui_value('pdap_write()')}")
    }
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

  package_check('keyring')

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

  package_check('keyring')

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

  package_check('keyring')

  # Attempt to get user to add account
  if(!is_stored("datim") & is_installed("keyring")) {

    user <- rstudioapi::askForPassword(prompt = "Enter your DATIM Username to setup your account (Close or Excape to abort):")
    user <- stringr::str_trim(user, side = "both")

    if (base::nchar(user) == 0)
      ui_stop("ERROR - Invalid username entered")

    set_datim(datim_username = user)
  }

  if(!is_stored("datim") & !is_installed("keyring"))
    ui_stop("NO DATIM credentials stored. Install `keyring` package and use {ui_code('set_datim()')} to setup datim account")

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

  package_check('keyring')

  if(!is_stored("datim"))
    ui_stop("NO DATIM credentials stored. Setup using {:ui_code('set_datim()')}")

  if(!is.loaded("datim"))
    suppressMessages(load_secrets())

  keyring::key_get("datim", getOption("datim"))

}


#' Store PEPFAR Panorama credentials
#'
#' `set_pano` stores your PEPFAR Panoram credentials email using the `keyring` package.
#' This will only need to done once. After running `set_pano(user)`, you will be
#' promoted to enter your password through the RStudio API which will then store the
#' username and password in your OS credential store using `keyring`.
#'
#'  The `keyring` package utilized the OS credentials store. Storing in
#'  a centralized, secure location allows analysts to other analysts code
#'  without having to manually change user names/email address to access
#'  DATIM, Panorama, or Google Drive.
#'
#'  After `set_pano` has been run once, an analyst can set `load_secrets` at the
#'  beginning of a script, storing their PEPFAR Panorama credentials under Options
#'  for the current session.
#'
#' @param pano_username Panorama user name (email)
#'
#' @return stores Panorama username and password in using keyring
#' @export
#' @family authentication
#'
#' @examples
#' \dontrun{
#' set_pano("rshah@usaid.gov") }
set_pano <- function(pano_username){

  package_check('keyring')

  keyring::key_set(service = "pano",
                   username = pano_username)
}


#' Return PEPFAR Panorama username
#'
#' To setup/store, run `glamr::set_pano()`.
#'
#' @return access Panorama username from keyring
#' @export
#' @family authentication
#'
#' @importFrom usethis ui_stop
#' @importFrom usethis ui_code
pano_user <- function(){

  package_check('keyring')

  if(!is_stored("pano"))
    ui_stop("NO Panorama credentials stored. Setup using {ui_code('set_pano()')}")

  if(!is.loaded("pano"))
    suppressMessages(load_secrets())

  keyring::key_list("pano")[1,2]

}

#' Return PEPFAR Panorama password
#'
#' To setup/store, run `glamr::set_pano()`.
#'
#' @return access Panorama password from keyring
#' @export
#' @family authentication
#'
#' @importFrom usethis ui_stop
#' @importFrom usethis ui_code
pano_pwd <- function(){

  package_check('keyring')

  if(!is_stored("pano"))
    ui_stop("NO Panorama credentials stored. Setup using {:ui_code('set_pano()')}")

  if(!is.loaded("pano"))
    suppressMessages(load_secrets())

  keyring::key_get("pano", getOption("pano"))

}

#' Store S3 Credentials
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

  package_check('keyring')

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
  package_check('keyring')
  keyring::key_get(service = "s3", username = name)
}

#' @title Get Services
#'
#' @return list of active services
#' @export
#' @family authentication
#'
#' @examples
#' \dontrun{
#' get_services()}
#'
get_services <- function() {
  keys <- keyring::key_list()
  unique(keys$service)
}

#' @title Get Service Keys
#'
#' @param service Account Service name
#'
#' @return list of key names for active services
#' @export
#' @family authentication
#'
#' @examples
#' \dontrun{
#' get_keys('<service-name>')}
#'
get_keys <- function(service) {
  keys <- keyring::key_list()
  keys[keys$service == service,]$username
}


#' @title Set value for service name
#'
#' @param service Name of the service
#' @param name    Name of the key
#'
#' @export
#' @family authentication
#'
#' @examples
#' \dontrun{
#' set_key(service = '<service-name>', name = '<key-name>')}
#'
set_key <- function(service, name) {

  msg <- glue::glue("Please enter value for {service}/{name} key:")

  value <- rstudioapi::askForPassword(prompt = msg)
  value <- stringr::str_trim(value, side = "both")

  if (base::nchar(value) == 0)
    base::stop("ERROR - Invalid value entered")

  keyring::key_set_with_value(service = service,
                              username = name,
                              password = value)
}


#' @title Get value of service key name
#'
#' @param service Name of the service
#' @param name    Name of the key
#'
#' @return key value
#' @export
#' @family authentication
#'
#' @examples
#' \dontrun{
#' get_key(service = '<service-name>', name = '<key-name>')}
#'
get_key <- function(service, name) {

  if (!service %in% get_services() | !name %in% get_keys(service)) {
    usethis::ui_warn("WARNINGS - Invalid service and/or key name")
    return(NULL)
  }

  keyring::key_get(service, name)
}

#' @title Get account details
#'
#' @note Inspired by `grabr::lazy_secrets()`
#'
#' @param name Service name of the account
#'
#' @return key / value pair as list containing details of the account (invisible)
#' @export
#' @family authentication
#'
#' @examples
#' \dontrun{
#' get_account(name = 's3')}
#'
get_account <- function(name) {

  package_check('keyring')

  if(!glamr::is_stored(name)) {

    if(!interactive())
      usethis::ui_stop("No {name} record found. Create a new account using {ui_code('set_key()')}")
  }

  accnt <- keyring::key_list(name)

  accnt %>%
    dplyr::pull(username) %>%
    purrr::map(function(username) {
      get_key(service = name, name = username)
    }) %>%
    purrr::set_names(accnt$username) %>%
    base::invisible()
}


#' @title Create / Update account
#'
#' @param name    Service name of the account
#' @param keys    List of account key names
#' @param update  Should an existing account be overwriten
#'
#' @export
#' @family authentication
#'
#' @examples
#' \dontrun{
#' set_account(name = 's3', keys = c("access", "secret"))}
#'
set_account <- function(name,
                        keys = c("username", "password"),
                        update = FALSE) {

  package_check('keyring')

  # Keyring Service
  srv <- name

  if(glamr::is_stored(srv) & !update) {
    usethis::ui_stop("{srv} exists already. Set update to TRUE to overwrite")
  }

  # Prompt user to set value for account keys
  keys %>%
    purrr::walk(function(key){
      set_key(service = srv, name = key)
    })
}



#' Store PDAP Access Key credentials
#'
#' When working with PDAP, you will need to access data from either the read or
#' write buckets and need the credentials to do so. This function stores the
#' Access Key associated with your account,
#' `Sys.getenv("AWS_ACCESS_KEY_ID")`. To use locally, the user will need to
#' store `set_key('pdap', 'access')`, which securely stores this information with
#' `keyring` (we can only write, not read from a local machine).
#'
#' @export
#' @family authentication
#'
#' \dontrun{
#' library(grabr)
#' s3_upload(upload_file_path,
#'           bucket = pdap_bucket("write"),
#'           prefix = "usaid/",
#'           access_key = pdap_access(),
#'           secret_key = pdap_secret())
#'
#' #identify path to dataset uploaded
#'   path_wrkbnch <- s3_objects(bucket = pdap_bucket("write"),
#'                              prefix = "usaid/",
#'                              access_key = pdap_access(),
#'                              secret_key = pdap_secret()) %>%
#'     filter(str_detect(key, "Moz")) %>%
#'     pull(key)
#'
#' #read
#' df_msd <- s3read_using(read_psd,
#'                        bucket = pdap_bucket("write"),
#'                        object = path_wrkbnch) }
#'
pdap_access <- function(){


  if(!is_pdap() && !is_stored("pdap"))
    ui_stop("NO PDAP access credentials stored. Setup using {ui_code('set_key(\\'pdap\\', \\'access\\')')}")

  if(!is_pdap() && !is.loaded("pdap_secret"))
    suppressMessages(load_secrets())

  if(is_pdap()){
    Sys.getenv("AWS_ACCESS_KEY_ID")
  } else {
    keyring::key_get("pdap", "access")
  }


}


#' Store PDAP Secret Access Key credentials
#'
#' When working with PDAP, you will need to access data from either the read or
#' write buckets and need the credentials to do so. This function stores the
#' Secret Access Key associated with your account,
#' `Sys.getenv("AWS_SECRET_ACCESS_KEY")`. To use locally, the user will need to
#' store `set_key('pdap', 'secret')`, which securely stores this information with
#' `keyring` (we can only write, not read from a local machine).
#'
#' @export
#' @family authentication
#'
#' \dontrun{
#' library(grabr)
#' s3_upload(upload_file_path,
#'           bucket = pdap_bucket("write"),
#'           prefix = "usaid/",
#'           access_key = pdap_access(),
#'           secret_key = pdap_secret())
#'
#' #identify path to dataset uploaded
#'   path_wrkbnch <- s3_objects(bucket = pdap_bucket("write"),
#'                              prefix = "usaid/",
#'                              access_key = pdap_access(),
#'                              secret_key = pdap_secret()) %>%
#'     filter(str_detect(key, "Moz")) %>%
#'     pull(key)
#'
#' #read
#' df_msd <- s3read_using(read_psd,
#'                        bucket = pdap_bucket("write"),
#'                        object = path_wrkbnch) }
#'
pdap_secret <- function(){

  if(!is_pdap() && !is_stored("pdap"))
    ui_stop("NO PDAP secrets key credentials stored. Setup access key using {ui_code('set_key(\\'pdap\\', \\'secret\\')')}")

  if(!is_pdap() && !is.loaded("pdap_secret"))
    suppressMessages(load_secrets())

  if(is_pdap()){
    Sys.getenv("AWS_SECRET_ACCESS_KEY")
  } else {
    keyring::key_get("pdap", "secret")
  }


}


#' Access PDAP read/write bucket
#'
#' When working with PDAP, you will need to access data from either the read or
#' write buckets. The read bucket ("S3_READ") is where PEPFAR Systems stores the
#' MSDs and the write bucket ("S3_WRITE") is where users can upload files (USAID
#' users will have access and write to the "usaid/" sub bucket).
#'
#' When access from PDAP Posit Workbench, the function will access the system
#' environment variables `Sys.getenv("S3_READ")` or `Sys.getenv("S3_WRITE")`
#' where as it accessing locally, the user will need to store the read bucket
#' location with `set_key()`, which securely stores this information with
#' `keyring` (we can only write, not read from a local machine).
#'
#' @param type is the bucket read (default) or write?
#'
#' @return character string of AWS bucket location
#' @export
#'
#' @family authentication
#'
#' @examples
#' \dontrun{
#' library(grabr)
#' s3_upload(upload_file_path,
#'           bucket = pdap_bucket("write"),
#'           prefix = "usaid/",
#'           access_key = pdap_access(),
#'           secret_key = pdap_secret())
#'
#' #identify path to dataset uploaded
#'   path_wrkbnch <- s3_objects(bucket = pdap_bucket("write"),
#'                              prefix = "usaid/",
#'                              access_key = pdap_access(),
#'                              secret_key = pdap_secret()) %>%
#'     filter(str_detect(key, "Moz")) %>%
#'     pull(key)
#'
#' #read
#' df_msd <- s3read_using(read_psd,
#'                        bucket = pdap_bucket("write"),
#'                        object = path_wrkbnch) }
#'
pdap_bucket <- function(type = c("read", "write")){

  type <- type[1]

  if(is_pdap()){
    b <- switch (type,
            "read" = Sys.getenv("S3_READ"),
            "write" = Sys.getenv("S3_WRITE")
    )

    return(b)
  }

  if(!is_pdap() && type == "read"){
    ui_oops("Reading PDAP data locally not accessible")
  }

  if(!is_pdap() && !is_stored("pdap"))
    ui_stop("NO PDAP write bucket location found Setup using {ui_code('set_key(\\'pdap\\', \\'write\\')')}")

  if(!is_pdap() && !is.loaded("pdap_write"))
    suppressMessages(load_secrets())

  keyring::key_get("pdap", "write")

}

