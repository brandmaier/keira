create_fake_scan <- function(nops_zip) {

  temp_dir <- paste0(tempdir(), "\\", "msbexams", sep = "")
  if (!file.exists(temp_dir))
    dir.create(temp_dir)

  # extract the zip file to the temporary directory
  unzip(zipfile = nops_zip,
        exdir = temp_dir,
        junkpaths = TRUE)
  path <- temp_dir

  daten <-
    read.csv(paste0(path,"\\","Daten.txt"),
             sep = " ",
             dec = ",",
             colClasses = "character", header=FALSE)

  colnames(daten)[1:6] <- c("scan","exam","scrambling","xxx","xxx","registration")

  fake_csv <- daten[,1:3] # only keep scan & exam

  for (i in 1:3) {
    fake_csv <- cbind(fake_csv, "00000")
  }

  colnames(fake_csv)[3+1:3] <- paste0("answer",1:3)

  write.table(x = fake_csv, col.names = FALSE, row.names=FALSE,
              file=paste0(path,"\\","Daten2.txt"),
              sep=" ",quote=FALSE)

  # wrap up everything in a ZIP file again
  zip(
    zipfile = paste0(nops_zip, "-fake.zip"),
    files = path,
    flags = "-jr"
  )
}
