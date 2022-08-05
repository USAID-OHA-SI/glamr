# glamr 1.1.0
* Fully depricate `my_pwd()` [2022-08-05]
* Migrate API related function (DATIM, Pano, s3) and vignette to `grabr`: `datim_dim_item`, `datim_dim_items`, `datim_dim_url`, `datim_dimension`, `datim_dimensions`, `datim_execute_query`, `datim_pops`, `datim_process_query`, `datim_query`, `get_levels`, `get_orguids`, `get_ouorglabel`, `get_ouorglevel`, `get_ouorgs`, `get_ouorguids`, `get_outable`, `get_ouuid`, `get_ouuids`, `identify_levels`, `identify_ouuids`, `lazy_secrets`, `package_check`, `pano_content`, `pano_download`, `pano_elements`, `pano_extract`, `pano_extract_msd`, `pano_extract_msds`, `pano_items`, `pano_session`, `s3_buckets`, `s3_download`, `s3_excel_sheets`, `s3_object_type`, `s3_objects`, `s3_read_object`, `s3_remove`, `s3_unpack_keys`, `s3_upload`
 [2022-08-05]

# glamr 1.0.5
* Update `pepfar_country_list` and `pepfar_country_xwalk` to reflect countries that receive any PEPFAR funding (budgets for the current FY) instead of just those that report MER indicators (2022-08-02)
* Add `pano_extract_msds` function to extract latest available MSDs from PEPFAR Panorama to local directory.
* Add `curr_date` function for on demand formatting of `Sys.Date()` values
* Add `open_path` function to help with navigating to directories and viewing with file content

# glamr 1.0.4
* Add `pluck_totals` function to filter dataframe to just Total Numerator and Denominator (2022-05-27)
* Update references to old variable names in `get_outtable`, `identify_levels`, `get_ouuids`, `get_ouuid`, `get_levels`, `get_ouorglevel`, `get_ouorglabel` and `resolve_knownissues` as well as with the raw data. The new names went into effect FY22Q2 (2022-04-08)
* Add DATIM UIDs to `pepfar_country_list` (2022-04-08)

# glamr 1.0.3
* Resolve grepel bug with `datim_dim_items` which kept the function from running (2022-04-15)
* Create a new period calculation, `convert_fy_qtr_to_pd`, using `fiscal_year` and quarter column to make it easier to create a period variable when not using `gophr::reshape_msd()`. (2022-04-07)
* Update both `pepfar_country_list` and `pepfar_country_xwalk` to include Benin, now reporting into PEPFAR starting in FY22 (2022-02-22)
* Update `source_info` to look for FY20 MSD filename as a default as opposed to FY19 (2022-02-22)

# glamr 1.0.2
* Resolve bug with `get_outable` which did not use the user defined base url when running `identify_ouuids`, resulting in a user without a final.datim.org account to not access the table.
* Fixed bug with `resolve_issues` that left of a column, breaking the function when run.
* Removed paging from `datim_dim_items` to return full set of dimensions
* Adjust `source_info` to handle dates for the FSD that are out of sync with the PEPFAR calendar.
* Included removal of central supporting from `resolve_knowissues` by default, which can also be run separately using `remove_centralsupport`.
* Update `resolve_knownissues` to handle working with financial data as well as MER.
* Has `resolve_knownissues` check Rprofile to see if authentication is stored there (from `set_email`) if not loaded in session.
* Update `clean_agency` to move State (State/AF, etc.) and USAID (USAID/WCF) subsidiaries under their parent agencies

# glamr 1.0.1
* Resolve bug with `resolve_knownissues` that resulted from having both targets and results for the same mechanism and indicator that need to be removed.
* Add set of functions to store, access, and load Panorama credentials - `set_pano`,
 `pano_user()`, `pano_pwd()`, `pano_session()`, and `load_secrets()`.
* Add functions to discover and download output datasets from Panorama: `pano_content()`, `pano_elements()`, `pano_unpack()`, `pano_extract()`, `pano_download()`, `pano_items()`, `pano_extract_msd()`
* Update `pepfar_data_calendar` with FY22 dates

# glamr 1.0.0
* Fixed `source_info()` to allow it to work correctly with FSD (misspelling in the filename).
* Allow user to just flag, not remove SCH mechanism in `remove_sch()`.
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
