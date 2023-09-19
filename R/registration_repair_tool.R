get_duplicates <- function(daten)
{
  registrations <- daten$V6

  dups <- duplicated(registrations)
  dups_rev <- duplicated(registrations, fromLast = TRUE)

  zeros <- (as.numeric(registrations) == 0)
  dups <- dups | dups_rev | zeros

  return(dups)
}


do_something <- function(daten, path, show_image, prop, d4) {
  dups <- get_duplicates(daten)

  if (any(dups)) {
    dupids <- which(dups)
    for (dupid in dupids) {
      cur_filename <- daten$V1[dupid]
      cur_registration <- daten$V6[dupid]
      cat(
        "Found duplicates for given registration ",
        toString(cur_registration),
        ". Please enter correct registration ID from picture."
      )
      temp_filename <- paste0(path, "//", cur_filename)
      #cur_png <- png::readPNG(temp_filename)

      if (show_image) {
        png_i <- examsMSB:::trim_nops_scan(temp_filename)

        png_i <-
          examsMSB:::subimage(png_i,
                              center = c(0.25, 0.87 - 0.04 * as.numeric(substr(d4, 1L, 1L))),
                              prop = prop)
        examsMSB:::imageplot(png_i, main = cur_filename)
      }
      new_reg <- readline(prompt = paste0("Enter ID: "))
      daten$V6[dupid] <- new_reg
    } # for id...
  } # if any

  return(daten)
}

registration_repair_tool <- function(path,
                                     prop = 0.5,
                                     d4 = 3,
                                     show_image = TRUE,
                                     recursive = TRUE,
                                     file_out = TRUE) {
  #path <- "misc/testsuite/45items/scans_png/nops_scan_20230713094857.zip"

  original_path <- path
  original_dir <- dirname(path)

  temp_dir <- paste0(tempdir(), "\\", "idc", sep = "")
  # extract the zip file to the temporary directory
  unzip(zipfile = path,
        exdir = temp_dir,
        junkpaths = TRUE)
  path <- temp_dir

  daten <- read.csv(
    paste0(path, "\\Daten.txt"),
    header = FALSE,
    colClasses = "character",
    sep = " "
  )

  correction_needed = TRUE
  while (correction_needed) {
    daten <- do_something(daten, path, show_image, prop, d4)

    # Do we need more corrections?
    dups <- get_duplicates(daten)
    correction_needed <- any(dups)
    if (correction_needed) {
      cat(
        "New mismatches discovered after correction. Please continue with corrections.\n"
      )
    }
  }

  # write result

  write.table(
    x = daten,
    file = paste0(path, "\\Daten.txt"),
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE,
    sep = " "
  )

  zip(
    zipfile = paste0(original_path, "-corrected.zip"),
    files = path,
    flags = "-jr"
  )


}
