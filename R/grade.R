
grade <- function(p, ml) {
  if (is.na(p)) return(NA)
  wch <- which(p<ml$marks)
  if (length(wch)==0) { return(ml$labels[length(ml$labels)])}
  else {
    wm <- min(wch)
    return(ml$label[wm])
  }
}
