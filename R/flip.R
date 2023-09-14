
flip_png <- function(x){
  original_matrix <- png::readPNG(x)
  flipped_matrix <- original_matrix[rev(seq_len(nrow(original_matrix))), rev(seq_len(ncol(original_matrix))), ]
  png::writePNG(flipped_matrix,target=paste0(x))
}

flip <- function(files)
{
  sapply(files, flip_png)
}
