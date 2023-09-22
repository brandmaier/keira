plot_points_distribution <- function(eval_file = "nops_eval.csv") {


  pts <- read.csv(eval_file,sep=";",dec=".")

  pnts <- pts$points


  ggplot2::ggplot(pts,ggplot2::aes(x=points))+
    ggplot2::geom_density()+
    ggplot2::geom_histogram()

}
