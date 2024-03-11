
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
