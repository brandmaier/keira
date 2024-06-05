clean_text <- function(x){
  return(tolower(unlist(strsplit(gsub("[^[:alnum:] ]", "", x), " +"))))
}

n_words <- function(x){
  return(sapply(x, function(y){length(clean_text(y))}))
}

n_chars <- function(x){
  return(nchar(x))
}

#' @description show some item statistics. Is there a bias towards correct or incorrect items w.r.t character or word counts?
#' @export
#'
plot_item_stats <- function() {

items = readRDS("testfile.rds")
num_items = length(items) / 3

cnts_all <- c()

for (i in 1:num_items) {

responses = items[[(i-1)*3+2]]
correct = items[[(i-1)*3+3]]
cnts = matrix(sapply(responses, function(x) { c(n_words(x), n_chars(x) ) },simplify = TRUE),ncol=2,byrow = TRUE)
cvc <- rep(0, length(responses))
cvc[correct]<-1
cnts <- cbind(cnts, cvc)

cnts_all <- rbind(cnts_all, cnts)



}

colnames(cnts_all)<-c("n_words","n_char","correct")

library(tidyverse)
cnts_all %>% as_tibble %>% group_by(correct) %>% summarise(mean(n_words),mean(n_char))


t.test(n_words~correct, data=cnts_all)
t.test(n_char~correct, data=cnts_all)

cnts_all %>% as_tibble %>% pivot_longer(1:2) %>%
  ggplot(aes(y=value,group=correct,fill=as.factor(correct)))+facet_wrap(~name)+geom_boxplot()

}
