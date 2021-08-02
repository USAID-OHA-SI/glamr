# glamr 1.0.0
* Used `pkgdown` to create a site
* Include two new function to convert dates to periods and periods to dates using `convert_date_to_qtr()` and `convert_qtr_to_date()`.
* New vigentte for project workflow added, `vignette(package = glamr)`
* Update `set_paths()`, allowing for `path_downloads` and improve backend efficiency.
* Store email in .Rprofile in addition to keyring using `set_email()` for use with `googledrive::drive_auth()` and `googlesheets4::gs4_auth()` without having to provide an email.
* Add `clean_filename()` function to remove apostrophe and other pesky characters from a filename, especially for use when uploading to Google Drive.
* Adjust `load_secrets()` to allow user to specify which accounts to load in a session.
* Depricate `mypwd()` which has been superseded by `datim_pwd()`.
* Create `source_info` function to extract information from the file to use in the caption/source.
* Adjust `setup_gitignore()` and `setup_readme()` to not duplicate text if its already exists
* Add a missing append to `setup_gitignore()` to not overwrite existing file.
* Add `pepfar_data_calendar` dataframe to be used in conjunction with source info.
* Added a `NEWS.md` file to track changes to the package.

# glamr 0.1.0
* Prior to verion 1.0.0, updates were not captured.
