# glamr 1.0.0
* Update `set_paths()`, allowing for `path_downloads` and improve backend efficiency
* Store email in .Rprofile in addition to keyring using `set_email()` for use with `googledrive::drive_auth()` and `googlesheets4::gs4_auth()` without having to provide an email
* Adjust `load_secrets()` to allow user to specify which accounts to load in a session
* Create `source_info` function to extract information from the file to use in the caption/source 
* Add `pepfar_data_calendar` dataframe to be used in conjunction with source info
* Added a `NEWS.md` file to track changes to the package.

# glamr 0.1.0
*Prior to verion 1.0.0, updates were not captured
