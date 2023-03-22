#
# grade report
#
path <- "raw_scan_png/"
outfolder <- "reports/"
yoffset <- 0

evalcsv <- read.csv("nops_eval.csv", sep=";",colClasses = "character")

nexams <- nrow(evalcsv)

nexams <- 3

for (i in 1:nexams) {

  id <- evalcsv$registration[i]
  as.character(id)

  fname <- evalcsv$scan[i]

  x <- png::readPNG(paste0(path,"/",fname,sep="",collapse=""))

  pixelwidth<- dim(x)[2]
  pixelheight <- dim(x)[1]

  startpos <- c( 330,1840) / c(2480, 3507)
  incx <- (420-330) / 2480
  incy <- (1920-1840) /3507

  extragapy <- (2700-2590 - 60-20) / 3507
  gapx1 <- c(1050-324) / 2480  # old between 1 and 2
  gapx2 <- (1730-1050) / 2480

  getLineCoor <- function (x1, y1, x2, y2)
  {
    steps <- max( abs(x1-x2), abs(y1-y2) ) + 1
    return ( cbind(round(seq(x1, x2, length.out=steps) ,0),
                   round(seq(y1, y2, length.out=steps) ,0)) )
  }

  GREEN <- c(0,1,0)
  RED <- c(1,0,0)

  drawLine <- function(x, x1, y1, x2, y2, lwd=1, col=RED) {
    for (j in 1:lwd) {
      coords <- getLineCoor(x1, y1+j, x2, y2+j)
      for (i in 1:nrow(coords)) {
        x[coords[i,2], coords[i,1],] <- col
      }
    }
    x
  }

  hatch <- function(x, x1, y1, x2, y2, lwd=8, col=RED)
  {
    x <- drawLine(x, x1,y1,x2,y2,lwd=lwd,col=col)
    x <- drawLine(x, x1,y2,x2,y1,lwd=lwd,col=col)
    x
  }


  rect <- function(x, x1, y1, x2, y2, lwd=8, col=RED, shrink=FALSE)
  {
    if (shrink) shrink = 1 else shrink = -1
    for (j in 1:lwd) {
      x <- drawLine(x,x1,y1,x1,y2, col=col)
      x <- drawLine(x,x2,y1,x2,y2, col=col)
      x <- drawLine(x,x1,y1,x2,y1, col=col)
      x <- drawLine(x,x1,y2,x2,y2, col=col)
      x1 <- x1 + shrink*1
      x2 <- x2 - shrink*1
      y1 <- y1 + shrink*1
      y2 <- y2 - shrink*1
    }
    x
  }

  #x <- drawLine(x,1840,330,1890,370, lwd=6)
  #x <- drawLine(x,1840,370,1890,330, lwd=6)

  #x <- rect(x, 1840, 330, 1890, 370, col=GREEN)

  resizer <- c(pixelwidth, pixelheight)

  # load info
  for (j in 1:45) {
    answer_header <- paste0("answer.",j,collapse = "")
    if (!(answer_header %in% names(evalcsv))) next
    answer_pattern <- evalcsv[i,answer_header]
    solution_pattern <- evalcsv[i,paste0("solution.",j,collapse = "")]
    cat("-> Item ",j,"\n")
    for (k in 1:5) {
      answer <- substr(answer_pattern,k,k)
      solution <- substr(solution_pattern,k,k)

      within_col_j <- (j-1)%%15+1
      column <- ((j-1) %/% 15)+1

      pos_x <- startpos[1] + (incx)*(k-1) #+ ((j-1) %/% 15)*gapx
      if (column == 2) pos_x <- pos_x + gapx1
      if (column == 3) pos_x <- pos_x + gapx1+gapx2
      pos_y <- startpos[2] + (incy)*(within_col_j-1) + ((within_col_j-1) %/% 5)*extragapy

      pos_x <- pos_x * pixelwidth
      pos_y <- pos_y * pixelheight + yoffset

      box_width <- 50
      box_height <- 42

      # correct checkmark -> green box
      if (answer==solution && answer=="1") {
        cat(" |- Correct answer at ",k," drawing at",pos_x,",",pos_y,"\n")
        x <- rect(x, pos_x, pos_y, pos_x+box_width, pos_y+box_height, col=GREEN)
      }

      # wrong checkmark -> red hatch
      if (answer=="1" && solution=="0")
      {
        cat(" |- Wrong answer at ",k," drawing at",pos_x,",",pos_y,"\n")
        x <- rect(x, pos_x, pos_y, pos_x+box_width, pos_y+box_height, col=RED)
      }

      # missed solution
      if (solution == "1" && answer=="0")
      {
        cat(" |- Missed answer at ",k," drawing at",pos_x,",",pos_y,"\n")
        x <- hatch(x,pos_x, pos_y, pos_x+box_width, pos_y+box_height, col=RED)
      }

    }

  }
  #x <- drawLine(x,1,500,100,500,lwd=10)

  outfile <- paste0(outfolder,"/",evalcsv$registration[i],".png",sep="",collapse = "")

  png::writePNG(x,outfile)

}
