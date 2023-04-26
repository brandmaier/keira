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
                         show_registration = TRUE,
                         show_exam = FALSE,
                         debug = FALSE,
                         hints=list(window_width=480,  yoffset = NA,
                                    xoffset = NA)) {

  xoffset <- hints$xoffset
  yoffset <- hints$yoffset

  path <- path_to_scans

  if(!file.exists(path)) {
    stop(paste0("File ",path," does not exist!"))
  }

  if (endsWith(path,".zip")) {
    temp_dir <- paste0(tempdir(),"\\","msbexams",sep="")
    if (!file.exists(temp_dir))  dir.create(temp_dir)

    # extract the zip file to the temporary directory
    unzip(zipfile=path, exdir = temp_dir, junkpaths = TRUE)
    path <- temp_dir
  }

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
    styler <- list(hatch=hatch, rect=myrect, correct_rejection=function(...){})

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
    plot(NULL, xlab="",ylab="",type="n", xlim=c(0, ncol(x)), ylim=c(0, nrow(x)), axes=FALSE, frame.plot=FALSE)
    rasterImage(x, 0, 0, ncol(x), nrow(x))

    if (show_exam) {
      text(200,3100, labels=paste0("Klausur-ID: ",evalcsv$exam[i]),cex=5,col="red")
    }
    if (show_registration) {
      text(200,3200, labels=paste0("Matrikelnummer: ",evalcsv$registration[i]),cex=5,col="red")
    }
    text(200,3300, labels=paste0("Note: ",evalcsv$mark[i]),cex=5,col="red")
    text(200,3400, labels=paste0("Punkte: ",round(as.numeric(evalcsv$points[i]),2)),cex=5,col="red")

    startpos <- c( -55, 1720) / c(2480, 3507)
    incx <- (420-330) / 2480
    incy <- (1920-1840) /3507

    extragapy <- (2700-2590 - 60-20) / 3507
    gapx1 <- c(1050-324) / 2480  # old between 1 and 2
    gapx2 <- (1730-1050) / 2480


   # if (debug) browser()
    topleft_match <- find_crosshair(x, starty=150, startx=0, window_width = 450)
    bottomright_match <- find_crosshair(x,starty = 3200,startx = 2000, window_width=400)

    rowmin <- topleft_match$rowmin
    colmin <- topleft_match$colmin

    #if (!found) {
    #  stop("Error! Could not detect crosshairs!")
    #}


    if (debug) {
      cat("Crosshair #",i,"(",evalcsv$registration[i],"): ",colmin,", ",rowmin,"\n")
      myrect(x1=colmin-20, y1=rowmin-20,x2=colmin+20,
             y2=rowmin+20, lwd=3, col = "blue", pixelheight=pixelheight)
      myrect(x1=0,x2=450,y1=150,y2=450,
             lwd=1,col="blue", pixelheight=pixelheight)

      colmin2 <- bottomright_match$colmin
      rowmin2 <- bottomright_match$rowmin
      myrect(x1=colmin2-20, y1=rowmin2-20,
             x2=colmin2+20, y2=rowmin2+20, lwd=3, col = "blue", pixelheight=pixelheight)
      myrect(x1=2000,x2=2000+400,
             y1=3200,y2=3500,lwd=1,col="blue", pixelheight=pixelheight)

      text(400,300, cex=5, labels=paste0("@(",rowmin,",",colmin,"); (",rowmin2,",",colmin2,")"), col="blue")

      w_diff <- colmin2-colmin
      h_diff <- rowmin2-rowmin
      text(400,500, cex=5, labels=paste0("WD: ",w_diff," HD: ",h_diff),col="blue")

    }

#    rowmin <- rowmin+200

    yoffset <- (rowmin-62)
    xoffset<- (colmin)

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

        # correct non-response
        if (answer==solution && answer=="0") {
          styler$correct_rejection(pos_x, pos_y, pos_x+box_width, pos_y+box_height, pixelheight=pixelheight)
        }

      }

    }



    dev.off()

    setTxtProgressBar(pb, i)

  }

  on.exit({})

}

find_crosshair <- function(x,
                           start_window_height=100,
                           max_window_height=300,
                           starty=200,
                           startx=0,
                           window_width=400) {

# find fixation cross
window_height <- start_window_height # from 100 to 300
found <- FALSE
while (!found && window_height <= max_window_height) {

  searchwindow_topleft_mark <- x[starty:(starty+window_height), startx:(startx+window_width),1]


#  rowmin <- which.min( (apply(searchwindow_topleft_mark, 1, mean)-0.9)^2 )  # Zeile
#  colmin <- which.min( (apply(searchwindow_topleft_mark, 2, mean)-0.85)^2)  # Spalte
  rowmin <- which.min( apply(searchwindow_topleft_mark, 1, mean))  # Zeile
  colmin <- which.min( apply(searchwindow_topleft_mark, 2, mean))  # Spalte

  window_height = window_height + 100

  minvalrow <- min((apply(searchwindow_topleft_mark, 1, mean)))

  if (minvalrow < 0.99) found <- TRUE
}

return(list(found=found, rowmin=starty+rowmin, colmin=startx+colmin))

}
