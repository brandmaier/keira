#'
#' @title grade report generator
#'
#' @description
#' Diese Funktion benötigt das Ergebnis einer Maschinenauswertung der Klausur,
#' typischerweise eine Datei mit dem Namen 'nops_eval.csv' und außerdem einen
#' Pfad zu einem Verzeichnis, in dem die gescannten Deckblätter liegen, die
#' für die Korrektur herangezogen wurden. Dazu entpackt man am Besten die ZIP-
#' Datei mit den Deckblättern, die bei der automatischen Korrektur erstellt
#' wurde und übergibt diesen Pfad an das Argument 'path_to_scans'.
#' Die Funktion grade_export() erzeugt dann korrigierte Deckblätter mit
#' Punkten und Note im PNG-Format. Standardmäßig wird ein Ordner 'reports'
#' erstellt, in dem diese Deckblätter abgelegt werden.
#'
#' @author Andreas M. Brandmaier
#'
#' @export

grade_report <- function(nops_eval_file="nops_eval.csv",
                         path_to_scans = "",
                         outfolder = "reports/",
                         graphics_format="png",
                         jpg_quality = 75,
                         res = NA,
                         styler = NULL,
                         show_points = FALSE,
                         debug = FALSE) {

  yoffset = 0
  xoffset = 0

  path <- path_to_scans

  if (!dir.exists(outfolder)) {
    dir.create(outfolder)
  }

  evalcsv <- read.csv(nops_eval_file, sep=";",dec = ",", colClasses = "character")

  nexams <- nrow(evalcsv)

  pb <- utils::txtProgressBar(max=nexams, style=2)


  # graphics helper function
  hatch <- function(x1, y1, x2, y2, lwd=8, col=RED, pixelheight=NULL)
  {
    y1 <- pixelheight - y1
    y2 <- pixelheight - y2
    lines( c(x1,x2),c(y1,y2), lwd=lwd, col=col)
    lines( c(x1,x2),c(y2,y1), lwd=lwd, col=col)
  }

  # graphics helper function
  myrect <- function(x1, y1, x2, y2, lwd=10, col="red", pixelheight=NULL) {

    #cat("Coords:", x1,pixelheight-y2,x2,pixelheight-y1,"\n")
    graphics::rect(x1,pixelheight-y2,x2,pixelheight-y1,lwd=10,border=col)
  }

  if (is.null(styler))
    styler <- list(hatch=hatch, rect=myrect)

  for (i in 1:nexams) {

    id <- evalcsv$registration[i]
    as.character(id)

    fname <- evalcsv$scan[i]

    on.exit({dev.off()})
    x <- png::readPNG(paste0(path,"/",fname,sep="",collapse=""))

    pixelwidth<- dim(x)[2]
    pixelheight <- dim(x)[1]

    if (graphics_format =="png")
      file_ending <- ".png"
    else if (graphics_format=="jpg" || graphics_format=="jpeg")
      file_ending <- ".jpg"
    else
      file_ending <- ""

    outfile <- paste0(outfolder,"/",evalcsv$registration[i],file_ending,sep="",collapse = "")

    if (graphics_format=="png") {
     # cat("PNG output\n")
      png(outfile,width = ncol(x),height=nrow(x), res = res)
    } else if (graphics_format=="jpg" || graphics_format=="jpeg") {
     # cat("JPG output\n")
      jpeg(outfile,width = ncol(x),height=nrow(x), quality=jpg_quality, res = res)
    } else {
      stop("Unknown graphics format! Try png or jpg")
    }
    plot(1, type="n", xlim=c(0, ncol(x)), ylim=c(0, nrow(x)), axes=FALSE, frame.plot=FALSE)
    rasterImage(x, 0, 0, ncol(x), nrow(x))

    text(200,3300, labels=paste0("Note: ",evalcsv$mark[i]),cex=5,col="red")
    text(200,3400, labels=paste0("Punkte: ",round(as.numeric(evalcsv$points[i]),2)),cex=5,col="red")

    startpos <- c( 330,1840) / c(2480, 3507)
    incx <- (420-330) / 2480
    incy <- (1920-1840) /3507

    extragapy <- (2700-2590 - 60-20) / 3507
    gapx1 <- c(1050-324) / 2480  # old between 1 and 2
    gapx2 <- (1730-1050) / 2480


    # find fixation cross
    window_height <- 100 # from 100 to 300
    found <- FALSE
    while (!found && window_height <= 300) {
      searchwindow_topleft_mark <- x[200:(200+window_height), 0:500,1]
      #image(searchwindow_topleft_mark)
      rowmin <- which.min( (apply(searchwindow_topleft_mark, 1, mean)-0.9)^2 )  # Zeile
      colmin <- which.min( (apply(searchwindow_topleft_mark, 2, mean)-0.85)^2)  # Spalte
      window_height = window_height + 100

      minvalrow <- min((apply(searchwindow_topleft_mark, 1, mean)))
      if (minvalrow < 0.99) found <- TRUE
    }

    if (debug) {
      cat("Crosshair #",i,"(",evalcsv$registration[i],"): ",colmin,", ",rowmin,"\n")
      myrect(x1=colmin-20, y1=200+rowmin-20,x2=colmin+20, y2=200+rowmin+20, lwd=3, col = "blue", pixelheight=pixelheight)
      myrect(x1=0,x2=500,y1=200,y2=500,lwd=1,col="blue", pixelheight=pixelheight)
    }

    yoffset <- (rowmin-62)
    #xoffset <- (colmin-395)

    resizer <- c(pixelwidth, pixelheight)

    # load info
    for (j in 1:45) {
      answercol <- paste0("answer.",j,collapse = "")
      if (!(answercol %in% names(evalcsv))) next
      answer_pattern <- evalcsv[i,answercol]
      solution_pattern <- evalcsv[i,paste0("solution.",j,collapse = "")]
      #cat("-> Item ",j,"\n")

      if (show_points) {
        within_col_j <- (j-1)%%15+1
        column <- ((j-1) %/% 15)+1
        pos_x <- startpos[1]
        if (column == 2) pos_x <- pos_x + gapx1
        if (column == 3) pos_x <- pos_x + gapx1+gapx2
        pos_y <- startpos[2] + (incy)*(within_col_j-1) + ((within_col_j-1) %/% 5)*extragapy
        pos_x <- pos_x * pixelwidth + xoffset
        pos_y <- pos_y * pixelheight + yoffset

        points <- round( as.numeric(evalcsv[i,paste0("points.",j,collapse="",sep="")]), 2)
        text(x=pos_x - 95, y=pixelheight-pos_y - 10, labels=paste0(points," P."),col="red",cex=2)
      }

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
          styler$rect(pos_x, pos_y, pos_x+box_width, pos_y+box_height, col="green", pixelheight=pixelheight)
        }

        # wrong checkmark -> red hatch
        if (answer=="1" && solution=="0")
        {
          #cat(" |- Wrong answer at ",k," drawing at",pos_x,",",pos_y,"\n")
          styler$rect(pos_x, pos_y, pos_x+box_width, pos_y+box_height, col="red", pixelheight=pixelheight)
        }

        # missed solution
        if (solution == "1" && answer=="0")
        {
          #cat(" |- Missed answer at ",k," drawing at",pos_x,",",pos_y,"\n")
          styler$hatch(pos_x, pos_y, pos_x+box_width, pos_y+box_height, col="red", pixelheight=pixelheight)
        }

      }

    }



    dev.off()

    setTxtProgressBar(pb, i)

  }

  on.exit({})

}
