latest_nops_zipfile <- function(path = "png") {
  files <- list.files(path = paste0(path, "/"), pattern = ".*nops_scan_.*zip", full.names = TRUE)
  dates <- sapply(files, function(x) {
    date_string <- (regmatches(x, regexpr("\\d+", x)))
    posix_date <- as.POSIXct(date_string, format = "%Y%m%d%H%M%S")
  }, simplify = FALSE)

  mtime <- sapply(files, function(file) {
    file.info(file)$mtime
  })

  files[which.max(mtime)]
}
