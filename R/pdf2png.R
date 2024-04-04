#' @export
pdf2png <- function(files,
                    output_dir = "",
                    dpi = 300) {
  if (dir.exists(files)) {
    files <- list.files(files, full.names = TRUE)
  }

  files <- sapply(files, normalizePath)

  if (output_dir != "") {
    wd <- getwd()
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    setwd(output_dir)
  }
  cat("Converting ", length(files), "files.\n")
  sapply(files, function(x) {
    pdftools::pdf_convert(x, format = "png", dpi = dpi)
  })

  if (output_dir != "") {
    setwd(wd)
  }
}
