# glamr 0.1.0

* Improvement to S3 utility functions: `s3_objects()`
* Extract metadata from googledirve files, `drive_ls(fldr) %>% gdrive_metadata()`
* Read sheets from excel files in S3, `s3_excel_sheets(bucket = "<sample-bucket>", object_key = "<sample-key>")
* Connect text for file name or other use, `connect_text()`


# glamr 0.0.0.9000

* Initial set of utility functions
* Import googledrive files `import_drivefile(fldr, "TestFile.csv")`
* List data object from S3 bucket `s3_objects("<sample-bucket>")`
