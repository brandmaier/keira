
percblack <- function(x, threshold = .25) {
  mean((x < 0.25)) # 1 = white, 0 = black
}

find_crosshair <- function(x,
                           start_window_height = 100,
                           max_window_height = 300,
                           starty = 200,
                           startx = 0,
                           window_width = 400,
                           height_increment = 100) {

  result <- .find_crosshair(x, start_window_height,
                            max_window_height,
                            starty,
                            startx,
                            window_width,
                            height_increment,
                            threshold = .25
                            )

  if (!result$found) {
    result <- .find_crosshair(x, start_window_height,
                              max_window_height,
                              starty,
                              startx,
                              window_width,
                              height_increment,
                              threshold = .25,
                              heuristic = "mean"
    )
  }

  return(result)
}

.find_crosshair <- function(x,
                           start_window_height = 100,
                           max_window_height = 300,
                           starty = 200,
                           startx = 0,
                           window_width = 400,
                           height_increment = 100,
                           threshold = 0.25,
                           heuristic = c("percblack","mean")) {

   heuristic = match.arg(heuristic)

  # find fixation cross
  window_height <- start_window_height # from 100 to 300
  found <- FALSE
  while (!found && window_height <= max_window_height) {
    searchwindow_topleft_mark <-
      x[starty:(starty + window_height), startx:(startx + window_width), 1]


  if (heuristic == "mean") {
    mean_whiteness_rowise <- apply(searchwindow_topleft_mark, 1, mean)
    mean_whiteness_colwise <- apply(searchwindow_topleft_mark, 2, mean)

     rowmin <-
       which.min( mean_whiteness_rowise )  # Zeile
     colmin <-
       which.min( mean_whiteness_colwise )  # Spalte

     # get row with largest amount of black
     minvalrow <- min((apply(searchwindow_topleft_mark, 1, mean)))

     # find id of that row
     minvalrow_id <- which.min((apply(searchwindow_topleft_mark, 1, mean)))

     if (minvalrow < 0.99) {
       found <- TRUE
     }

  } else {
    percblack_rowise <- apply(searchwindow_topleft_mark, 1, percblack, threshold=threshold)
    percblack_colwise <- apply(searchwindow_topleft_mark, 2, percblack, threshold=threshold)

    rowmin <- which.max( percblack_rowise )
    colmin <- which.max( percblack_colwise )

    maxval <- max(percblack_rowise)
    if (maxval > 0.10) {
      found <- TRUE
    }
  }

    window_height = window_height + height_increment
  }

  return(list(
    found = found,
    rowmin = starty + rowmin,
    colmin = startx + colmin
  ))

}
