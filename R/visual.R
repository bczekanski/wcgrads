visual <- function(x) {

#' @title visual
#' @description visual() makes a graphic showing the frequency of two variables and their marginal distributions.
#' It is intended to be used to show distribution of initials
#' @param x The data to be plotted
#' @return A graph showing the joint and marginal distribution of initials
#' @usage visual(x)
#' @export
#' @import ggplot2 ggExtra dplyr

#Make main plot

a <- x %>%
  ggplot(aes(firstinit, lastinit)) +
  geom_bin2d() +
  scale_fill_gradient2()

#Add Marginal Plots

b <- ggMarginal(a, x, x$firstinit, x$lastinit, type = "histogram", margins = "both", stat = "count")

ggsave(filename = "vignettes/initials.png", plot=b)

return(b)

}
