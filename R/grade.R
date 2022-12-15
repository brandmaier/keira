#
# input: a ZIP file containing PNG scans of exams
# output: a ZIP file containing PNG scans and a machine-read score file
#
#

grade <- function(file) {

  # unzip file
  unzip(file, junkpaths=TRUE, exdir="temp_nops_scan")

  # do the scan, this creates a ZIP file containing PNG files
  # and the scanned results
  nops_scan(dir = "temp_nops_scan",verbose = TRUE,file = "output.zip")




}
