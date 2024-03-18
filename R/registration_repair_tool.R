get_duplicates <- function(daten)
{
  registrations <- daten$V6

  if (any(registrations=="")) {
    registrations[registrations==""] <- "0"
  }

  dups <- duplicated(registrations)
  dups_rev <- duplicated(registrations, fromLast = TRUE)

  zeros <- (as.numeric(registrations) == 0)
  dups <- dups | dups_rev | zeros

  return(dups)
}


fix_registration <- function(daten, path, show_image, prop, d4) {
  dups <- get_duplicates(daten)

  if (any(dups)) {
    cat("I found ", sum(dups)," problems. Let's fix them together!\n")
    dupids <- which(dups)
    for (dupid in dupids) {
      cur_filename <- daten$V1[dupid]
      cur_registration <- daten$V6[dupid]
      cat(
        "Found problems with ID",
        toString(cur_registration),
        ". Please enter correct registration ID from picture."
      )
      temp_filename <- paste0(path, "//", cur_filename)
      #cur_png <- png::readPNG(temp_filename)

      # this should happen only on readout errors, when the
      # position marks couldn't be read out prroperly
      if (toString(cur_registration)=="") {
        cat("\n /!\ Caught irrecoverable error in file ", cur_filename, ". Please manually fix PDF and rerun! Probably, the position marks were unreadable.\n")
        stop()
      }

      if (show_image) {
        png_i <- NULL
        tryCatch({
        png_i <- examsMSB:::trim_nops_scan(temp_filename)
        },catch=function(e){
          cat("exams package found an error when opening PDF ",cur_filename,":\n\n")
          print(e)
          cat("--------\n")
        })
        if (is.null(png_i)) {next;}
        png_i <-
          examsMSB:::subimage(png_i,
                              center = c(0.25, 0.87 - 0.04 * as.numeric(substr(d4, 1L, 1L))),
                              prop = prop)
        examsMSB:::imageplot(png_i, main = cur_filename)
      }
      new_reg <- readline(prompt = paste0("Enter ID: "))
      if (show_image) { dev.off() }
      daten$V6[dupid] <- new_reg
    } # for id...
  } # if any

  return(daten)
}

#'
#' this is a nifty helper function to fix problems
#' with answer sheets that have missing information
#' or appear to be duplicates (because of faulty info)
#'
#' @param prop
#' @param d4
#' @param show_image  Boolean. Default: TRUE. Show an excerpt of the registration
#'
registration_repair_tool <- function(path=latest_nops_zipfile(),
                                     prop = 0.5,
                                     d4 = 3,
                                     show_image = TRUE,
                                     recursive = TRUE,
                                     file_out = TRUE) {

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
    daten <- fix_registration(daten, path, show_image, prop, d4)

    # Do we need more corrections?
    dups <- get_duplicates(daten)
    correction_needed <- any(dups)
    if (correction_needed) {
      cat(
        "New mismatches discovered after correction. Please continue with corrections.\n"
      )
    }
  }

  # write result to Daten.txt
  write.table(
    x = daten,
    file = paste0(path, "\\Daten.txt"),
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE,
    sep = " "
  )

  # wrap up everything in a ZIP file again
  zip(
    zipfile = paste0(original_path, "-corrected.zip"),
    files = path,
    flags = "-jr"
  )


}
