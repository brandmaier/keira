#
# grade report
#
path <- "processing/nops_scan_20230321153245/"
outfolder <- "reports/"
yoffset <- 0
xoffset <- 0
evalcsv <- read.csv("nops_eval.csv", sep=";",colClasses = "character")

nexams <- nrow(evalcsv)

#nexams <- 20


hatch <- function(x1, y1, x2, y2, lwd=8, col=RED)
{
  y1 <- pixelheight - y1
  y2 <- pixelheight - y2
  lines( c(x1,x2),c(y1,y2), lwd=8, col=col)
  lines( c(x1,x2),c(y2,y1), lwd=8, col=col)
}

myrect <- function(x1, y1, x2, y2, lwd=2, col="red") {

  #cat("Coords:", x1,pixelheight-y2,x2,pixelheight-y1,"\n")
  graphics::rect(x1,pixelheight-y2,x2,pixelheight-y1,lwd=10,border=col)
}

for (i in 1:nexams) {

  id <- evalcsv$registration[i]
  as.character(id)

  fname <- evalcsv$scan[i]

  x <- png::readPNG(paste0(path,"/",fname,sep="",collapse=""))

  pixelwidth<- dim(x)[2]
  pixelheight <- dim(x)[1]

  outfile <- paste0(outfolder,"/",evalcsv$registration[i],".png",sep="",collapse = "")
  png(outfile,width = ncol(x),height=nrow(x))
  plot(1, type="n", xlim=c(0, ncol(x)), ylim=c(0, nrow(x)), axes=FALSE, frame.plot=FALSE)
  rasterImage(x, 0, 0, ncol(x), nrow(x))

  text(200,3300, labels=paste0("Note: ",evalcsv$mark[i]),cex=5,col="red")
  text(200,3400, labels=paste0("Punkte: ",round(as.numeric(evalcsv$points[i]))),cex=5,col="red")

  startpos <- c( 330,1840) / c(2480, 3507)
  incx <- (420-330) / 2480
  incy <- (1920-1840) /3507

  extragapy <- (2700-2590 - 60-20) / 3507
  gapx1 <- c(1050-324) / 2480  # old between 1 and 2
  gapx2 <- (1730-1050) / 2480


  RED <- "red"
    GREEN <- "green"

    # find fixation cross
    searchwindow_topleft_mark <- x[200:500, 0:500,1]
    #image(searchwindow_topleft_mark)
    rowmin <- which.min( (apply(searchwindow_topleft_mark, 1, mean)-0.6)^2 )  # Zeile
    colmin <- which.min( (apply(searchwindow_topleft_mark, 2, mean)-0.85)^2)  # Spalte


    cat("Crosshair #",i,"(",evalcsv$registration[i],"): ",colmin,", ",rowmin,"\n")
    myrect(x1=colmin-20, y1=200+rowmin-20,x2=colmin+20, y2=200+rowmin+20, lwd=3, col = "blue")
    myrect(x1=0,x2=500,y1=200,y2=500,lwd=1,col="blue")

    resizer <- c(pixelwidth, pixelheight)

    # load info
    for (j in 1:45) {
      answercol <- paste0("answer.",j,collapse = "")
      if (!(answercol %in% names(evalcsv))) next
      answer_pattern <- evalcsv[i,answercol]
      solution_pattern <- evalcsv[i,paste0("solution.",j,collapse = "")]
      #cat("-> Item ",j,"\n")
      for (k in 1:5) {
        answer <- substr(answer_pattern,k,k)
        solution <- substr(solution_pattern,k,k)

        within_col_j <- (j-1)%%15+1
        column <- ((j-1) %/% 15)+1

        pos_x <- startpos[1] + (incx)*(k-1) #+ ((j-1) %/% 15)*gapx
        if (column == 2) pos_x <- pos_x + gapx1
        if (column == 3) pos_x <- pos_x + gapx1+gapx2
        pos_y <- startpos[2] + (incy)*(within_col_j-1) + ((within_col_j-1) %/% 5)*extragapy

        pos_x <- pos_x * pixelwidth + xoffset
        pos_y <- pos_y * pixelheight + yoffset

        box_width <- 50
        box_height <- 44

        # correct checkmark -> green box
        if (answer==solution && answer=="1") {
          #cat(" |- Correct answer at ",k," drawing at",pos_x,",",pos_y,"\n")
          myrect(pos_x, pos_y, pos_x+box_width, pos_y+box_height, col=GREEN)
        }

        # wrong checkmark -> red hatch
        if (answer=="1" && solution=="0")
        {
          #cat(" |- Wrong answer at ",k," drawing at",pos_x,",",pos_y,"\n")
          myrect(pos_x, pos_y, pos_x+box_width, pos_y+box_height, col=RED)
        }

        # missed solution
        if (solution == "1" && answer=="0")
        {
          #cat(" |- Missed answer at ",k," drawing at",pos_x,",",pos_y,"\n")
          hatch(pos_x, pos_y, pos_x+box_width, pos_y+box_height, col=RED)
        }

      }

    }



    dev.off()

}
