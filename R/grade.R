
grade <- function(p, ml) {
  wch <- which(p<ml$marks)
  if (is.null(wch)) { return(ml$labels[length(ml$labels)])}
  else {
    wm <- min(wch)
    return(ml$label[wm])
  }
}
