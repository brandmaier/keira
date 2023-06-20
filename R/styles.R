# graphics helper function
hatch <- function(x1, y1, x2, y2, lwd=8, col=RED,  pixelheight = NULL)
{
  y1 <- pixelheight - y1
  y2 <- pixelheight - y2
  lines( c(x1,x2),c(y1,y2), lwd=lwd, col=col)
  lines( c(x1,x2),c(y2,y1), lwd=lwd, col=col)
}

# graphics helper function
myrect <- function(x1, y1, x2, y2, lwd=10, col="red", pixelheight=NULL) {

  if (col=="red") {
    graphics::rect(x1,pixelheight-y2,x2,pixelheight-y1,lwd=lwd*.7,border=col,col="#FF000040")
  } else {
    graphics::rect(x1,pixelheight-y2,x2,pixelheight-y1,lwd=lwd*.4,border=col,col="#00FF0040")
  }
}



style_semitransparent <- list(hatch=hatch, rect=myrect,   correct_rejection = function(...) {
})
