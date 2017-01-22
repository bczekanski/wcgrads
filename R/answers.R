gradcount <- function() {

#' @title
#' @description
#' @param
#' @return
#' @usage
#' @import dplyr gender
#' @export

a <- allyrs %>%
  group_by(grad.year) %>%
  summarize(count = n())

b <- a %>%
  ggplot(aes(grad.year, count)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Graduating Seniors Over Time") +
  xlab("Graduation Year") +
  ylab("Graduating Seniors")


return(b)

}

namelength <- function() {

#' @title
#' @description
#' @param
#' @return
#' @usage
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
