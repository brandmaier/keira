#' graphics helper function
#'
#' @export
#'
hatch <- function(x1, y1, x2, y2, lwd = 8, col = RED, pixelheight = NULL) {
  y1 <- pixelheight - y1
  y2 <- pixelheight - y2
  lines(c(x1, x2), c(y1, y2), lwd = lwd, col = col)
  lines(c(x1, x2), c(y2, y1), lwd = lwd, col = col)
}

#'  graphics helper function
#'
#' @export
myrect <- function(x1, y1, x2, y2, lwd = 10, col = "red", pixelheight = NULL) {
  if (type == "warn") {
    graphics::rect(x1, pixelheight - y2, x2, pixelheight - y1, lwd = lwd * .7, border = col, col = "yellow")
    return()
  }

  if (col == "red") {
    graphics::rect(x1, pixelheight - y2, x2, pixelheight - y1, lwd = lwd * .7, border = col, col = "#FF000040")
  } else {
    graphics::rect(x1, pixelheight - y2, x2, pixelheight - y1, lwd = lwd * .4, border = col, col = "#00FF0040")
  }
}



style_semitransparent_old <- list(
  hatch = hatch,
  rect = myrect,
  correct_rejection = function(...) {
  }
)


style_semitransparent <- list(
  hatch = function(x1, y1, x2, y2, lwd = 8, col = RED, pixelheight = NULL) {
    y1 <- pixelheight - y1
    y2 <- pixelheight - y2
    lines(c(x1, x2), c(y1, y2), lwd = lwd, col = "#FF000090")
    lines(c(x1, x2), c(y2, y1), lwd = lwd, col = "#FF000090")
  },
  rect = function(x1, y1, x2, y2, lwd = 10, col = "red", pixelheight = NULL) {
    if (type == "warn") {
      graphics::rect(x1, pixelheight - y2, x2, pixelheight - y1, lwd = lwd * .7, border = col, col = "yellow")
      return()
    }

    if (col == "red") {
      graphics::rect(x1, pixelheight - y2, x2, pixelheight - y1, lwd = lwd * .7, border = col, col = "#FF000020")
    } else {
      graphics::rect(x1, pixelheight - y2, x2, pixelheight - y1, lwd = lwd * .4, border = col, col = "#00FF0020")
    }
  },
  correct_rejection = function(...) {}
)



style_cbfriendly <- list(
  hatch = function(x1, y1, x2, y2, lwd = 8, col = RED, pixelheight = NULL) {
    y1 <- pixelheight - y1
    y2 <- pixelheight - y2
    lines(c(x1, x2), c(y1, y2), lwd = lwd, col = "#fc8d62")
    lines(c(x1, x2), c(y2, y1), lwd = lwd, col = "#fc8d62")
  },
  rect = function(x1, y1, x2, y2, lwd = 10, col = "red", pixelheight = NULL, type = "warn") {
    if (type == "warn") {
      graphics::rect(x1, pixelheight - y2, x2, pixelheight - y1, lwd = lwd * .7, border = col, col = "#8da0cb70")
      return()
    }
    if (col == "red") {
      graphics::rect(x1, pixelheight - y2, x2, pixelheight - y1, lwd = lwd * .7, border = col, col = "#fc8d6270")
    } else {
      graphics::rect(x1, pixelheight - y2, x2, pixelheight - y1, lwd = lwd * .4, border = col, col = "#66c2a570")
    }
  },
  correct_rejection = function(...) {}
)
