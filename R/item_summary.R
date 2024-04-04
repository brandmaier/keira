item_summary <- function() {
  itd <- item_discrimination()

  num_items <- length(itd)

  df <- data.frame(paste0("item", 1:num_items), itd)

  names(df) <- c("Item", "Discrimination")

  return(df)
}
