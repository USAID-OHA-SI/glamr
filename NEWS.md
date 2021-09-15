# glamr 1.0.0
* Resolve bug with new code for `set_path`, causing an error if any path was missing from the .Rprofile
* The new `temp_folder()` creates and stores a folder in your Global Envir to use, which will be automatically be deleted on the end of your RStudio session.
* Used `pkgdown` to create a site
* If no username/password are provided to `get_outable()`, `identify_ouuids()` or `identify_levels()`, defaults to using `datim_user()` and `datim_pwd()`
* Adjust `get_outable()`/`identify_levels()` output, changing community to community_lvl and prioritization to psnu_lvl to match other indicators.
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
* Add `pepfar_data_calendar` data frame to be used in conjunction with source info.
* Add `pepfar_country_list` data frame to have a set of PEPFAR countries and their ISO codes. 
* Add `pepfar_country_xwalk` data frame as a cross-walk for PEPFAR, UN, CountryCode country names
* Add `datim_*` functions for datim queries
* Edited `clean_countries()` to account for short version of PEPFAR OU/Countries
* Added a `NEWS.md` file to track changes to the package.

# glamr 0.1.0
* Prior to verion 1.0.0, updates were not captured.
