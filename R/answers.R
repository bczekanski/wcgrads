gradcount <- function() {

#' @title Graduate Count
#' @description gradcount gets the count of graduates each year and graphs the information
#' @return A bar graph of graduating seniors by year
#' @usage gradcount()
#' @import dplyr ggplot2
#' @export

a <- allyrs %>%
  group_by(grad.year) %>%
  summarize(count = n())

b <- a %>%
  ggplot(aes(grad.year, count)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Graduating Seniors Over Time") +
  xlab("Graduation Year") +
  ylab("Graduating Seniors") +
  labs(caption = "This chart shows the raw number of graduates from Williams College, by year.
       2000 had the most graduates while 2005 had the fewest graduates. ")


return(b)

}

namelength <- function() {

#' @title Name Length
#' @description namelength counts the number of letters in each name
#' @return A data frame of all graduates with their name, the number of characters and their graduation year
#' @usage namelength()
#' @import dplyr stringr
#' @export

a <- allyrs %>%
  mutate(entirename = paste(first, middle, last, sep = " ")) %>%
  mutate(entirename = gsub(" NA ", " ", entirename)) %>%
  mutate(nospaces = paste0(first, middle, last)) %>%
  mutate(nospaces = gsub("NA", "", nospaces)) %>%
  mutate(nospaces = gsub(" ", "", nospaces)) %>%
  mutate(nospaces = gsub("'", "", nospaces)) %>%
  mutate(nospaces = gsub("-", "", nospaces)) %>%
  mutate(namelength = str_length(nospaces)) %>%
  select(entirename, namelength, grad.year) %>%
  arrange(desc(namelength))

return(a)

}
