

plot_item_difficulty <- function(eval_file = "nops_eval.csv",
                                 maxpoints = 1,
                                 cols=c("green", "blue", "red")) {
  pts <- eval_file

  pcols_id <-
    which(sapply(names(pts), function(x) {
      startsWith(x, "points.")
    }))
  n_items <- length(pcols_id)
  subdat <- pts[, c(1:8, pcols_id)]

  color_table <- tibble(
    lab = c("Difficult", "Normal", "Easy"),
    col = cols
  )

  names(subdat)[1:n_items + 8] <-
    paste0("Item ", stringr::str_pad(1:n_items, pad = "0", width = 3))
  npts <-
    subdat[, -(1:8)] %>% pivot_longer(1:n_items) %>% group_by(name) %>% summarise(avg =
                                                                                    mean(value)) %>% mutate(avgn = avg / maxpoints)
  npts$cl <- ifelse(npts$avgn <= 0.10, "Difficult", "Normal")
  npts$cl <- ifelse(npts$avgn >= 0.90, "Easy", npts$cl)
  npts$cl <- factor(npts$cl, levels = c("Easy", "Normal", "Difficult"))
  npts %>% ggplot(aes(avgn, name, fill = cl)) + geom_col() + xlab("Schwierigkeit") +
    ylab("Item") +
    scale_fill_manual(values = color_table$col, drop = FALSE) + xlim(0, 1)

}
