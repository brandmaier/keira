item_discrimination <- function() {
eval_file <- "nops_eval.csv"
pts <- read.csv(eval_file,sep=";",dec=",")

pcols_id <-
  which(sapply(names(pts), function(x) {
    startsWith(x, "points.")
  }))
n_items <- length(pcols_id)
subdat <- pts[, c(pcols_id)]

disc <- sapply(1:n_items, function(i){
score <- rowSums(subdat[,-i])
item <- subdat[,i]
r <- cor(score,item)
})

return(disc)
}
