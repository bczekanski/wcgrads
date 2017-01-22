latin_honors <- function(){

#' @title
#' @description
#' @param
#' @return
#' @usage
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
  mutate(yrly.pct = count/yrly.count)

z <- a %>%
  ggplot(aes(grad.year, yrly.pct, fill = latin.honors)) +
     geom_bar(stat = "identity", position = "dodge") +
     ggtitle("Latin Honors") +
     xlab("Graduation Year") +
     ylab("Percent Receiving Latin Honors") +
     guides(fill = guide_legend(title = "Honors Level"))


return(z)

}

total_latin_honors <- function(){

#' @title
#' @description
#' @param
#' @return
#' @usage
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
  mutate(yrly.pct = count/yrly.count) %>%
  mutate(yrly.pct = ifelse(latin.honors == "Magna Cum Laude", yrly.pct + lead(yrly.pct), yrly.pct)) %>%
  mutate(yrly.pct = ifelse(latin.honors == "Cum Laude", yrly.pct + lead(yrly.pct), yrly.pct))

c <- b %>%
  ggplot(aes(grad.year, yrly.pct, fill = latin.honors)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = c(.02, .15, .35)) +
  ggtitle("Cumulative Latin Honors") +
  xlab("Graduation Year") +
  ylab("Cumulative Percent Receiving Latin Honors") +
  guides(fill = guide_legend(title = "Honors Level"))


return(c)

}
