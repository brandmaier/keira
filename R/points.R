extract_points <- function(files, schoice=1, mchoice=2) {
  sapply(files, FUN=function(f) {
    ll<-readLines(f)
    infer_points(ll, schoice, mchoice)
  })
}

extract_number <- function(string) {
  number <- gsub("[^0-9.]", "", string)
  as.numeric(number)
}

infer_points <- function(ll, schoice, mchoice) {
  #browser()
  cmpexp <- startsWith(ll, "\\expoints")
  has_expoints_line <- any(cmpexp)
  if (!has_expoints_line) {
    # infer points
    cmpext <- startsWith(ll, "\\extype")
    if (sum(cmpext)!=1) stop("No unique extype given!")
    extl <- ll[cmpext]
    if (extl=="\\extype{schoice}") {
      p <- schoice
    } else if (extl=="\\extype{mchoice}") {
      p <- mchoice
    } else {
      p <- NA
    }
  } else {
    # read out points
    if (sum(cmpexp)>1) stop("Error: Multiple expoints given!")
    exl <- ll[cmpexp]
    p <- extract_points(exl)
  }
  return(p)
}
