registration_repair_tool <- function(path,
                                     prop=0.5,
                                     d4=3,
                                     show_image=TRUE) {
  #path <- "misc/testsuite/45items/scans_png/nops_scan_20230713094857.zip"

  original_path <- path
  original_dir <- dirname(path)

  temp_dir <- paste0(tempdir(), "\\", "idc", sep = "")
  # extract the zip file to the temporary directory
  unzip(zipfile = path,
        exdir = temp_dir,
        junkpaths = TRUE)
  path <- temp_dir

  daten <- read.csv(paste0(path, "\\Daten.txt"),
                    header = FALSE, colClasses = "character",
                    sep = " ")

  registrations <- daten$V6

  dups <- duplicated(registrations)

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

      #dpng <- dim(png_i)
      #image((png_i[1000:1500,1000:1500]))

      png_i <-
        examsMSB:::subimage(png_i,
                            center = c(0.25, 0.87 - 0.04 * as.numeric(substr(d4, 1L, 1L))),
                            prop = prop)
      examsMSB:::imageplot(png_i, main = cur_filename)
      }
      new_reg <- readline(prompt = paste0("Corrected: "))
      daten$V6[dupid] <- new_reg
    }

      write.table(x=daten,file = paste0(path, "\\Daten.txt"),
                      col.names=FALSE, row.names=FALSE, quote=FALSE,
                      sep = " ")

    zip(zipfile = paste0(original_path, "-corrected.zip"), files = path, flags="-jr")

    return(FALSE)
  } else {
    return(TRUE)
  }



}
