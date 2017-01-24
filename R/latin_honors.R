latin_honors <- function(){

#' @title Latin Honors
#' @description latin_honors shows the percentage of each graduating class that is awarded latin honors.
#' @return A bar graph showing the percentage of each graduating class awarded each level of latin honors.
#' @usage latin_honors()
#' @import dplyr ggplot2
#' @export

a <- allyrs %>%
  select(grad.year, latin.honors) %>%
  group_by(grad.year, latin.honors) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(grad.year) %>%
  mutate(yrly.count = sum(count)) %>%
  filter(is.na(latin.honors) == FALSE) %>%
  mutate(yrly.pct = 100*count/yrly.count)

z <- a %>%
  ggplot(aes(grad.year, yrly.pct, fill = latin.honors)) +
     geom_bar(stat = "identity", position = "dodge") +
     ggtitle("Latin Honors") +
     xlab("Graduation Year") +
     ylab("Percent Receiving Latin Honors") +
     guides(fill = guide_legend(title = "Honors Level")) +
     labs(caption = "The percentages of graduating seniors who received each level
           of latin honors. There is no overall trend but they seem to be related
          in their variation, due to the phrase 'at least' in the guidelines.")


return(z)

}

total_latin_honors <- function(){

#' @title Total Latin Honors
#' @description total_latin_honors shows cumulative latin honors for comparison with guidelines given by the Williams College Faculty.
#' @return A bar graph showing the percentage of each graduating class attaining at least each level of latin honors.
#' There are also black lines that indicate the guidelines given in the Williams College Course Catalog.
#' @usage total_latin_honors()
#' @import dplyr ggplot2
#' @export

b <- allyrs %>%
  select(grad.year, latin.honors) %>%
  group_by(grad.year, latin.honors) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(grad.year) %>%
  mutate(yrly.count = sum(count)) %>%
  filter(is.na(latin.honors) == FALSE) %>%
  mutate(yrly.pct = 100*count/yrly.count) %>%
  mutate(yrly.pct = ifelse(latin.honors == "Magna Cum Laude", yrly.pct + lead(yrly.pct), yrly.pct)) %>%
  mutate(yrly.pct = ifelse(latin.honors == "Cum Laude", yrly.pct + lead(yrly.pct), yrly.pct))

c <- b %>%
  ggplot(aes(grad.year, yrly.pct, fill = latin.honors)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = c(2, 15, 35)) +
  ggtitle("Cumulative Latin Honors") +
  xlab("Graduation Year") +
  ylab("Cumulative Percent Receiving Latin Honors") +
  guides(fill = guide_legend(title = "Honors Level")) +
  labs(caption = "This figure shows the percentage of seniors that received at least each
level of latin honors.These levels are compared to the guidelines set forth in the Course
       Catalogs, which are denoted by the black lines.")


return(c)

}
