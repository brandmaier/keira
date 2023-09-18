
flip_png <- function(x){
  original_matrix <- png::readPNG(x)
  flipped_matrix <- original_matrix[rev(seq_len(nrow(original_matrix))), rev(seq_len(ncol(original_matrix))), ]
  png::writePNG(flipped_matrix,target=normalizePath(x))
}

flip <- function(files)
{
  if (dir.exists(files)) {
    files <- list.files(files, full.names = TRUE)
  }

  sapply(files, flip_png)
}
