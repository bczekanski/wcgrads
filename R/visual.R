visual <- function(x) {

library(wcgrads)
library(dplyr)
library(ggplot2)
library(ggExtra)
#' make plot
a <- x %>%
  ggplot(aes(firstinit, lastinit)) +
  geom_bin2d() +
  scale_fill_gradient2()

b <- ggMarginal(a, x, x$firstinit, x$lastinit, type = "histogram", margins = "both", stat = "count")
b

}
