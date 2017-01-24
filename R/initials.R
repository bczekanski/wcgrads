initials <- function() {

#' @title Initials
#' @description initials() makes a graphic showing the frequency of initials and the marginal distributions of first and last initials.
#' @return A graph showing the joint and marginal distribution of initials
#' @usage initials()
#' @export
#' @import ggplot2 ggExtra dplyr

#Make main plot
u <- allyrs %>%
  select(first, last, grad.year) %>%
  mutate(firstinit = toupper(substr(gsub("[^[:alnum:] ]", "", first), 1, 1))) %>%
  mutate(lastinit = toupper(substr(last, 1, 1)))

a <- u %>%
  ggplot(aes(firstinit, lastinit)) +
  geom_bin2d() +
  scale_fill_gradient2() +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'), legend.position = "left") +
  ylab("Last Initial") +
  xlab("First Initial") +
  ggtitle("Initials of Williams College Graduates (2000-2016)") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(position = "right") +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  labs(caption = "This figure shows the distribution of first and last initials among
                 Williams College Graduates. A and J are the most popular first initials
                  while S appears to be the most frequent last initial.")

#Add Marginal Plots

b <- ggMarginal(a, u, u$firstinit, u$lastinit, type = "histogram", margins = "both", stat = "count", xparams = list(labels = TRUE))

return(b)

}
