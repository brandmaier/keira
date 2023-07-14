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

grade_report <- function(nops_eval_file = "nops_eval.csv",
                         path_to_scans = "",
                         outfolder = "reports/",
                         graphics_format = "png",
                         jpg_quality = 75,
                         res = NA,
                         style = style_semitransparent,
                         show_points = FALSE,
                         show_points_total = FALSE,
                         show_registration = TRUE,
                         show_exam = FALSE,
                         debug = FALSE,
                         hints = list(window_width = 480,
                                      yoffset = NA,
                                      xoffset = NA,
                                      extragapy_rel = NA,
                                      gapx1 = NA,
                                      gapx2 = NA)) {
  xoffset <- hints$xoffset
  yoffset <- hints$yoffset

  path <- path_to_scans

  if (!file.exists(path)) {
    stop(paste0("File ", path, " does not exist!"))
  }

  if (endsWith(path, ".zip")) {
    temp_dir <- paste0(tempdir(), "\\", "msbexams", sep = "")
    if (!file.exists(temp_dir))
      dir.create(temp_dir)

    # extract the zip file to the temporary directory
    unzip(zipfile = path,
          exdir = temp_dir,
          junkpaths = TRUE)
    path <- temp_dir
  }

  if (!dir.exists(outfolder)) {
    dir.create(outfolder)
  }

  evalcsv <-
    read.csv(nops_eval_file,
             sep = ";",
             dec = ",",
             colClasses = "character")

  nexams <- nrow(evalcsv)

  pb <- utils::txtProgressBar(max = nexams, style = 2)


  # graphics helper function
  hatch <-
    function(x1,
             y1,
             x2,
             y2,
             lwd = 8,
             col = RED,
             pixelheight = NULL)
    {
      y1 <- pixelheight - y1
      y2 <- pixelheight - y2
      lines(c(x1, x2), c(y1, y2), lwd = lwd, col = col)
      lines(c(x1, x2), c(y2, y1), lwd = lwd, col = col)
    }

  # graphics helper function
  myrect <-
    function(x1,
             y1,
             x2,
             y2,
             lwd = 10,
             col = "red",
             pixelheight = NULL) {
      graphics::rect(x1,
                     pixelheight - y2,
                     x2,
                     pixelheight - y1,
                     lwd = lwd,
                     border = col)
    }

  if (is.null(style))
    style <-
    list(
      hatch = hatch,
      rect = myrect,
      correct_rejection = function(...) {
      }
    )

  for (i in 1:nexams) {
    id <- evalcsv$registration[i]
    as.character(id)

    fname <- evalcsv$scan[i]

    on.exit({
      dev.off()
    })
    x <- png::readPNG(paste0(path, "/", fname, sep = "", collapse = ""))

    pixelwidth <- dim(x)[2]
    pixelheight <- dim(x)[1]


    scaling_factor_x <- pixelwidth / 2480 # <1 if lower resolution
    scaling_factor_y <- pixelheight / 3507 # <1 if lower resolution

    scaling_cex <- scaling_factor_y

    if (pixelwidth < 2476 | pixelwidth > 2484) {
      warning(
        paste0(
          "The report generator expects a page width of 2480 pixel, that is, a A4 scan with 300 DPI. The pixel size of the current document is ",
          pixelwidth,
          " pixels.\nContinuing with scaling factors ",scaling_factor_x," and ",scaling_factor_y,"."
        )
      )
    }

    if (!test_fuzzy_equal(scaling_factor_x, scaling_factor_y, pdiff = 2)) {
      stop("Error! X and Y scaling factors differ substantially (>2%).")
    }

    if (graphics_format == "png")
      file_ending <- ".png"
    else if (graphics_format == "jpg" || graphics_format == "jpeg")
      file_ending <- ".jpg"
    else
      file_ending <- ""

    outfile <-
      paste0(
        outfolder,
        "/",
        evalcsv$registration[i],
        file_ending,
        sep = "",
        collapse = ""
      )

    if (graphics_format == "png") {

      png(
        outfile,
        width = ncol(x),
        height = nrow(x),
        res = res
      )
    } else if (graphics_format == "jpg" ||
               graphics_format == "jpeg") {

      jpeg(
        outfile,
        width = ncol(x),
        height = nrow(x),
        quality = jpg_quality,
        res = res
      )
    } else {
      stop("Unknown graphics format! Try png or jpg")
    }
    plot(
      NULL,
      xlab = "",
      ylab = "",
      type = "n",
      xlim = c(0, ncol(x)),
      ylim = c(0, nrow(x)),
      axes = FALSE,
      frame.plot = FALSE
    )
    rasterImage(x, 0, 0, ncol(x), nrow(x))

    if (show_exam) {
      text(
        200 * scaling_factor_x,
        3100 * scaling_factor_y,
        labels = paste0("Klausur-ID: ", evalcsv$exam[i]),
        cex = 5 * scaling_cex,
        col = "red"
      )
    }
    if (show_registration) {
      text(
#        200 * scaling_factor_x,
#        3200 * scaling_factor_y,
        1200 * scaling_factor_x,
        3400 * scaling_factor_y,
        labels = paste0("Matrikelnummer: ", evalcsv$registration[i]),
        cex = 5 * scaling_cex,
        col = "red"
      )
    }
    text(
      200 * scaling_factor_x,
      3300 * scaling_factor_y,
      labels = paste0("Note: ", evalcsv$mark[i]),
      cex = 5 * scaling_cex,
      col = "red"
    )
    text(
      200 * scaling_factor_x,
      3400 * scaling_factor_y,
      labels = paste0("Punkte: ", round(as.numeric(
        evalcsv$points[i]
      ), 2)),
      cex = 5 * scaling_cex,
      col = "red"
    )

    startpos_rel <- c(-55, 1720) / c(2480, 3507) #* c(scaling_factor_x, scaling_factor_y)
    incx_rel <- (420 - 330) / 2480 #* scaling_factor_x
    incy_rel <- (1920 - 1840) / 3507 #* scaling_factor_y

    #
    # extragapy is the (relative) height of the white space between every block of 5 items
    #
    #extragapy <- (2700 - 2590 - 60 - 20) / 3507 #* scaling_factor_y   # used to work but too small now
    if (is.na(hints$extragapy_rel)) {
      extragapy_rel <- (2700 - 2590 - 60 - 20 + 20) / 3507  # this works on PDF->PNG(300DPI) without scanning
    } else {
      extragapy_rel <- hints$extragapy_rel
    }

    #gapx1 <- c(1050 - 324) / 2480# * scaling_factor_x  # old between 1 and 2
    #gapx2 <- (1730 - 1050) / 2480 #* scaling_factor_x
    if (is.na(hints$gapx1)) {
      gapx1 <- c(1050 - 324 + 30) / 2480# * scaling_factor_x  # old between 1 and 2
    } else {
      gapx1 <- hints$gapx1
    }
    if (is.na(hints$gapx2)) {
      gapx2 <- (1730 - 1050 + 25) / 2480 #* scaling_factor_x
    } else {
      gapx2 <- hints$gapx2
    }

    # if (debug) browser()
    topleft_match <-
      find_crosshair(x,
                     starty = (150* scaling_factor_y),
                     startx = 0,
                     window_width = (450* scaling_factor_x),
                     height_increment = 100 * scaling_factor_y)

    bottomright_match <-
      find_crosshair(x,
                     starty = (3200 * scaling_factor_y),
                     startx = (2000 * scaling_factor_x),
                     window_width = (400* scaling_factor_x),
                     height_increment = 100 * scaling_factor_y)

    rowmin <- topleft_match$rowmin
    colmin <- topleft_match$colmin

    #if (!found) {
    #  stop("Error! Could not detect crosshairs!")
    #}


    if (debug) {
      cat("Crosshair #",
          i,
          "(",
          evalcsv$registration[i],
          "): ",
          colmin,
          ", ",
          rowmin,
          "\n")
      # indicate top left crosshair
      myrect(
        x1 = colmin - 20,
        y1 = rowmin - 20,
        x2 = colmin + 20,
        y2 = rowmin + 20,
        lwd = 3,
        col = "blue",
        pixelheight = pixelheight
      )
      # indicate search window
      myrect(
        x1 = 0,
        x2 = 450*scaling_factor_x,
        y1 = 150*scaling_factor_y,
        y2 = 450*scaling_factor_y,
        lwd = 1,
        col = "blue",
        pixelheight = pixelheight
      )

      colmin2 <- bottomright_match$colmin
      rowmin2 <- bottomright_match$rowmin
      myrect(
        x1 = colmin2 - 20,
        y1 = rowmin2 - 20,
        x2 = colmin2 + 20,
        y2 = rowmin2 + 20,
        lwd = 3,
        col = "blue",
        pixelheight = pixelheight
      )
      # indicate search window
      myrect(
        x1 = 2000*scaling_factor_x,
        x2 = (2000 + 400)*scaling_factor_x,
        y1 = 3200*scaling_factor_x,
        y2 = 3500*scaling_factor_y,
        lwd = 1,
        col = "blue",
        pixelheight = pixelheight
      )

      text(
        400*scaling_factor_x,
        300*scaling_factor_y,
        cex = 5 * scaling_cex,
        labels = paste0("@(", rowmin, ",", colmin, "); (", rowmin2, ",", colmin2, ")"),
        col = "blue"
      )

      w_diff <- colmin2 - colmin
      h_diff <- rowmin2 - rowmin
      text(
        400*scaling_factor_x,
        500*scaling_factor_y,
        cex = 5 * scaling_cex,
        labels = paste0("WD: ", w_diff, " HD: ", h_diff),
        col = "blue"
      )

    }
    # -- end debug output --

   # scaling_factor_y <- 1
    #scaling_factor_x <- 1

    yoffset <- (rowmin - (62 * scaling_factor_y))
    xoffset <- (colmin)

    #myrect( xoffset-30,
    #        yoffset-30,
    #        xoffset+30,
    #        yoffset+30,
    #        lwd=1,
    #        col="purple",pixelheight=2)

    #resizer <- c(pixelwidth, pixelheight)

    # load info
    for (j in 1:45) {
      answercol <- paste0("answer.", j, collapse = "")
      if (!(answercol %in% names(evalcsv)))
        next
      answer_pattern <- evalcsv[i, answercol]
      solution_pattern <-
        evalcsv[i, paste0("solution.", j, collapse = "")]

      if (show_points || show_points_total) {
        within_col_j <- (j - 1) %% 15 + 1
        column <- ((j - 1) %/% 15) + 1
        pos_x <- startpos_rel[1]
        if (column == 2)
          pos_x <- pos_x + gapx1
        if (column == 3)
          pos_x <- pos_x + gapx1 + gapx2
        pos_y <-
          startpos_rel[2] + (incy_rel) * (within_col_j - 1) + ((within_col_j - 1) %/% 5) *
          extragapy_rel
        pos_x <- pos_x * pixelwidth + xoffset
        pos_y <- pos_y * pixelheight + yoffset

        points <-
          round(as.numeric(evalcsv[i, paste0("points.", j, collapse = "", sep = "")]), 2)

        if (show_points && !show_points_total) {
          points_label <- (paste0(points, " P."))
        } else {

        }

        text(
          x = pos_x - (95 * scaling_factor_x),
          y = pixelheight - pos_y - (10 * scaling_factor_y),
          labels = points_label,
          col = "red",
          cex = 2 * scaling_cex
        )
      }

      for (k in 1:5) {
        answer <- substr(answer_pattern, k, k)
        solution <- substr(solution_pattern, k, k)

        within_col_j <- (j - 1) %% 15 + 1
        column <- ((j - 1) %/% 15) + 1

        pos_x <- startpos_rel[1] + (incx_rel) * (k - 1) #+ ((j-1) %/% 15)*gapx
        if (column == 2)
          pos_x <- pos_x + gapx1
        if (column == 3)
          pos_x <- pos_x + gapx1 + gapx2
        pos_y <-
          startpos_rel[2] + (incy_rel) * (within_col_j - 1) + ((within_col_j - 1) %/% 5) *
          extragapy_rel

        pos_x <- pos_x * pixelwidth + xoffset
        pos_y <- pos_y * pixelheight + yoffset

        box_width <- (50 * scaling_factor_x)
        box_height <- (44 * scaling_factor_y)

        # correct checkmark -> green box
        if (answer == solution && answer == "1") {
          #cat(" |- Correct answer at ",k," drawing at",pos_x,",",pos_y,"\n")
          style$rect(
            pos_x,
            pos_y,
            pos_x + box_width,
            pos_y + box_height,
            col = "green",
            pixelheight = pixelheight,
            lwd = 10 * scaling_cex
          )
        }

        # wrong checkmark -> red hatch
        if (answer == "1" && solution == "0")
        {
          #cat(" |- Wrong answer at ",k," drawing at",pos_x,",",pos_y,"\n")
          style$rect(
            pos_x,
            pos_y,
            pos_x + box_width,
            pos_y + box_height,
            col = "red",
            pixelheight = pixelheight,
            lwd = 10 * scaling_cex
          )
        }

        # missed solution
        if (solution == "1" && answer == "0")
        {
          #cat(" |- Missed answer at ",k," drawing at",pos_x,",",pos_y,"\n")
          style$hatch(
            pos_x,
            pos_y,
            pos_x + box_width,
            pos_y + box_height,
            col = "red",
            pixelheight = pixelheight,
            lwd = 8 * scaling_cex
          )
        }

        # correct non-response
        if (answer == solution && answer == "0") {
          style$correct_rejection(pos_x,
                                   pos_y,
                                   pos_x + box_width,
                                   pos_y + box_height,
                                   pixelheight = pixelheight)
        }

      }

    }



    dev.off()

    setTxtProgressBar(pb, i)

  }

  on.exit({
  })

}

find_crosshair <- function(x,
                           start_window_height = 100,
                           max_window_height = 300,
                           starty = 200,
                           startx = 0,
                           window_width = 400,
                           height_increment = 100) {
  # find fixation cross
  window_height <- start_window_height # from 100 to 300
  found <- FALSE
  while (!found && window_height <= max_window_height) {
    searchwindow_topleft_mark <-
      x[starty:(starty + window_height), startx:(startx + window_width), 1]

    rowmin <-
      which.min(apply(searchwindow_topleft_mark, 1, mean))  # Zeile
    colmin <-
      which.min(apply(searchwindow_topleft_mark, 2, mean))  # Spalte

    window_height = window_height + height_increment

    minvalrow <- min((apply(searchwindow_topleft_mark, 1, mean)))

    if (minvalrow < 0.99)
      found <- TRUE
  }

  return(list(
    found = found,
    rowmin = starty + rowmin,
    colmin = startx + colmin
  ))

}


test_fuzzy_equal <- function(value1, value2, pdiff = 1) {
  percentage_diff <-
    abs(value1 - value2) / ((value1 + value2) / 2) * 100
  return (percentage_diff < pdiff)
}
