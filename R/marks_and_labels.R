getMarksAndLabels <- function(maxpoints) {
#  maxpoints <-
  labels <- c("n.b.","4,0","3,7","3,3","3,0","2,7","2,3","2,0","1,7","1,3","1,0")
  marks <- c(0.5, 0.545, 0.606, 0.652, 0.697, 0.758, 0.803, 0.848, 0.909, 0.955)*maxpoints
  marks <- round(marks*2)/2 # auf halbe Stellen gerundet
  return( list(labels=labels, marks=marks))
}
