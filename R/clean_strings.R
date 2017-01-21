clean1 <- function(file){

#' @title Clean Data Part One
#' @description clean1()
#' @param file
#' @return
#' @usage
#' @import stringr dplyr tidyr
#' @export

g <- file %>%
  rename(strings = value) %>%
  mutate(strings = iconv(strings, from = "ISO-8859-1", to = "UTF-8")) %>%
  mutate(strings = gsub('[0-9]', "", strings)) %>%
  mutate(strings = gsub(" \f", "", strings)) %>%
  mutate(pbk = str_detect(strings, fixed("*", TRUE))) %>%
  separate(strings, c("name", "honors"), sep = ", with") %>%
  mutate(name = ifelse(str_count(name, " ") == 0, gsub(fixed("-", TRUE), fixed(" ", TRUE), name), name)) %>%
  separate(name, c("first", "other"), sep = " ", extra = "merge") %>%
  mutate(first = sub(fixed("[*]", TRUE), "", first)) %>%
  mutate(first = sub(fixed("[+]", TRUE), "", first)) %>%
  mutate(last = word(other, -1)) %>%
  mutate(suffix = ifelse(last == "I" | last == "II" | last == "III" | last == "IV" | last == "V" | last == "Jr.", last, NA)) %>%
  mutate(last = ifelse(is.na(suffix) == FALSE, word(other, -2), last)) %>%
  mutate(middle = ifelse(is.na(suffix) == FALSE, word(other, 1, -3), word(other, 1, -2))) %>%
  mutate(alpha = gsub(c(" ","'"), "", last)) %>%
  mutate(alpha = gsub("'", "", alpha)) %>%
  mutate(alpha = tolower(alpha))

r <- as.numeric(which(str_detect(g$first, "Bachelor") == TRUE))

g$latin.honors <- ifelse(row(g) <= r[3], "Cum Laude", NA)
g$latin.honors <- ifelse(row(g) <= r[2], "Magna Cum Laude", g$latin.honors)
g$latin.honors <- ifelse(row(g) <= r[1], "Summa Cum Laude", g$latin.honors)

g <- g %>%
  filter(str_detect(first, "Bachelor") == FALSE)

return(g)

}

clean2 <- function(file){

#' @title
#' @description
#' @param
#' @return
#' @usage
#' @import dplyr tidyr
#' @export

  i <- file %>%
    mutate(honors.id = lead(alpha, 1, default = "0") <= lag(alpha, 1, default = "0")) %>%
    mutate(honors.id2 = lead(alpha, 2, default = "0") <= lag(alpha, 2, default = "0")) %>%
    mutate(honors.id3 = lead(alpha, 3, default = "0") <= lag(alpha, 3, default = "0")) %>%
    mutate(honors.id4 = lead(alpha, 4, default = "0") <= lag(alpha, 4, default = "0")) %>%
    mutate(honors.id5 = lead(alpha, 5, default = "0") <= lag(alpha, 5, default = "0")) %>%
    mutate(honors.id6 = lead(alpha, 10, default = "0") <= lag(alpha, 10, default = "0")) %>%
    mutate(honors.sum = honors.id + honors.id2 + honors.id3 + honors.id4 + honors.id5 + honors.id6) %>%
    mutate(split.wrong = alpha < lag(alpha, default = "0") | alpha > lead(alpha, default = "0")) %>%
    mutate(split.wrong2 = alpha < lag(alpha, 2, default = "0") | alpha > lead(alpha, 2, default = "0")) %>%
    mutate(split.wrong3 = alpha < lag(alpha, 3, default = "0") | alpha > lead(alpha, 3, default = "0")) %>%
    mutate(split.wrong4 = alpha < lag(alpha, 4, default = "0") | alpha > lead(alpha, 4, default = "0")) %>%
    mutate(split.wrong5 = alpha < lag(alpha, 5, default = "0") | alpha > lead(alpha, 5, default = "0")) %>%
    mutate(split.wrong10 = alpha < lag(alpha, 10, default = "0") | alpha > lead(alpha, 10, default = "0")) %>%
    mutate(sw.sum = split.wrong + split.wrong2 + split.wrong3 + split.wrong4 + split.wrong5 +split.wrong10) %>%
    mutate(y = (sw.sum >3 & honors.sum <3)) %>%
    mutate(switched = (y == TRUE & first >= lag(alpha) & first <= lead(alpha) & str_count(other, " ") == 0)) %>%
    mutate(first = ifelse(switched == TRUE, paste(first, last, sep = "#"), first)) %>%
    mutate(last = ifelse(switched == TRUE, word(first, 1, sep = "#"), last)) %>%
    mutate(first = ifelse(switched == TRUE, word(first, -1, sep = "#"), first)) %>%
    mutate(other = ifelse(switched == TRUE, NA, other)) %>%
    mutate(alpha = gsub(c(" ","'"), "", last)) %>%
    mutate(alpha = gsub("'", "", alpha)) %>%
    mutate(alpha = tolower(alpha))

  h <- i %>%
    mutate(honors.id = lead(alpha, 1, default = "0") <= lag(alpha, 1, default = "0")) %>%
    mutate(honors.id2 = lead(alpha, 2, default = "0") <= lag(alpha, 2, default = "0")) %>%
    mutate(honors.id3 = lead(alpha, 3, default = "0") <= lag(alpha, 3, default = "0")) %>%
    mutate(honors.id4 = lead(alpha, 4, default = "0") <= lag(alpha, 4, default = "0")) %>%
    mutate(honors.id5 = lead(alpha, 5, default = "0") <= lag(alpha, 5, default = "0")) %>%
    mutate(honors.id6 = lead(alpha, 10, default = "0") <= lag(alpha, 10, default = "0")) %>%
    mutate(honors.sum = honors.id + honors.id2 + honors.id3 + honors.id4 + honors.id5 + honors.id6) %>%
    mutate(split.wrong = alpha < lag(alpha, default = "0") | alpha > lead(alpha, default = "0")) %>%
    mutate(split.wrong2 = alpha < lag(alpha, 2, default = "0") | alpha > lead(alpha, 2, default = "0")) %>%
    mutate(split.wrong3 = alpha < lag(alpha, 3, default = "0") | alpha > lead(alpha, 3, default = "0")) %>%
    mutate(split.wrong4 = alpha < lag(alpha, 4, default = "0") | alpha > lead(alpha, 4, default = "0")) %>%
    mutate(split.wrong5 = alpha < lag(alpha, 5, default = "0") | alpha > lead(alpha, 5, default = "0")) %>%
    mutate(split.wrong10 = alpha < lag(alpha, 10, default = "0") | alpha > lead(alpha, 10, default = "0")) %>%
    mutate(sw.sum = split.wrong + split.wrong2 + split.wrong3 + split.wrong4 + split.wrong5 +split.wrong10) %>%
    mutate(y = (sw.sum >3 & honors.sum <3)) %>%
    mutate(last = ifelse(y == TRUE, word(other,-2, -1), last)) %>%
    mutate(middle = ifelse(y == TRUE, word(other, 1, -3), middle)) %>%
    mutate(alpha = gsub(c(" ","'"), "", last)) %>%
    mutate(alpha = gsub("'", "", alpha)) %>%
    mutate(alpha = tolower(alpha))

  g <- h %>%
    mutate(honors.id = lead(alpha, 1, default = "0") <= lag(alpha, 1, default = "0")) %>%
    mutate(honors.id2 = lead(alpha, 2, default = "0") <= lag(alpha, 2, default = "0")) %>%
    mutate(honors.id3 = lead(alpha, 3, default = "0") <= lag(alpha, 3, default = "0")) %>%
    mutate(honors.id4 = lead(alpha, 4, default = "0") <= lag(alpha, 4, default = "0")) %>%
    mutate(honors.id5 = lead(alpha, 5, default = "0") <= lag(alpha, 5, default = "0")) %>%
    mutate(honors.id6 = lead(alpha, 10, default = "0") <= lag(alpha, 10, default = "0")) %>%
    mutate(honors.sum = honors.id + honors.id2 + honors.id3 + honors.id4 + honors.id5 + honors.id6) %>%
    mutate(split.wrong = alpha < lag(alpha, default = "0") | alpha > lead(alpha, default = "0")) %>%
    mutate(split.wrong2 = alpha < lag(alpha, 2, default = "0") | alpha > lead(alpha, 2, default = "0")) %>%
    mutate(split.wrong3 = alpha < lag(alpha, 3, default = "0") | alpha > lead(alpha, 3, default = "0")) %>%
    mutate(split.wrong4 = alpha < lag(alpha, 4, default = "0") | alpha > lead(alpha, 4, default = "0")) %>%
    mutate(split.wrong5 = alpha < lag(alpha, 5, default = "0") | alpha > lead(alpha, 5, default = "0")) %>%
    mutate(split.wrong10 = alpha < lag(alpha, 10, default = "0") | alpha > lead(alpha, 10, default = "0")) %>%
    mutate(sw.sum = split.wrong + split.wrong2 + split.wrong3 + split.wrong4 + split.wrong5 +split.wrong10) %>%
    mutate(y = (sw.sum >3 & honors.sum <3)) %>%
    mutate(last = ifelse(y == TRUE, word(other,-3, -1), last)) %>%
    mutate(middle = ifelse(y == TRUE, word(other, 1, -4), middle)) %>%
    mutate(alpha = gsub(c(" ","'"), "", last)) %>%
    mutate(alpha = gsub("'", "", alpha)) %>%
    mutate(alpha = tolower(alpha))

  f <- g %>%
    mutate(honors.id = lead(alpha, 1, default = "0") <= lag(alpha, 1, default = "0")) %>%
    mutate(honors.id2 = lead(alpha, 2, default = "0") <= lag(alpha, 2, default = "0")) %>%
    mutate(honors.id3 = lead(alpha, 3, default = "0") <= lag(alpha, 3, default = "0")) %>%
    mutate(honors.id4 = lead(alpha, 4, default = "0") <= lag(alpha, 4, default = "0")) %>%
    mutate(honors.id5 = lead(alpha, 5, default = "0") <= lag(alpha, 5, default = "0")) %>%
    mutate(honors.id6 = lead(alpha, 10, default = "0") <= lag(alpha, 10, default = "0")) %>%
    mutate(honors.sum = honors.id + honors.id2 + honors.id3 + honors.id4 + honors.id5 + honors.id6) %>%
    mutate(split.wrong = alpha < lag(alpha, default = "0") | alpha > lead(alpha, default = "0")) %>%
    mutate(split.wrong2 = alpha < lag(alpha, 2, default = "0") | alpha > lead(alpha, 2, default = "0")) %>%
    mutate(split.wrong3 = alpha < lag(alpha, 3, default = "0") | alpha > lead(alpha, 3, default = "0")) %>%
    mutate(split.wrong4 = alpha < lag(alpha, 4, default = "0") | alpha > lead(alpha, 4, default = "0")) %>%
    mutate(split.wrong5 = alpha < lag(alpha, 5, default = "0") | alpha > lead(alpha, 5, default = "0")) %>%
    mutate(split.wrong10 = alpha < lag(alpha, 10, default = "0") | alpha > lead(alpha, 10, default = "0")) %>%
    mutate(sw.sum = split.wrong + split.wrong2 + split.wrong3 + split.wrong4 + split.wrong5 +split.wrong10) %>%
    mutate(y = (sw.sum >3 & honors.sum <3)) %>%
    mutate(last = ifelse(y == TRUE, word(other, -4, -1), last)) %>%
    mutate(middle = ifelse(y == TRUE, word(other, 1, -5), middle))

  return(f)

}

clean3 <- function(file, year){

#' @title
#' @description
#' @param
#' @return
#' @usage
#' @import dplyr tidyr
#' @export

gradyear <- as.numeric(paste0("20", year)) - 1

if(gradyear == 2014) {

g <- file %>%
  mutate(grad.year = gradyear) %>%
  mutate(last = sub(fixed("[,]", TRUE), "", last)) %>%
  separate(honors, c("first.honors", "second.honors"), sep = "and with") %>%
  separate(first.honors, c("first.honors.level", "first.honors.dept"), sep = " in") %>%
  separate(second.honors, c("second.honors.level", "second.honors.dept"), sep = " in") %>%
  select(-honors.id2, -honors.id3, -honors.id4, -honors.id5, -honors.id6, -honors.sum,
           -split.wrong2, -split.wrong3, -split.wrong4, -split.wrong5, -split.wrong10, -sw.sum, -alpha)

}

else if(gradyear == 2005 | gradyear == 2008 | gradyear == 2013) {

g <- file %>%
  mutate(grad.year = gradyear) %>%
  mutate(last = sub(fixed("[,]", TRUE), "", last)) %>%
  mutate(second.honors.dept = ifelse(str_detect(honors, "and") == TRUE &
                                       str_detect(honors, "Contract") == FALSE &
                                       str_detect(honors, "Studies") == FALSE,
                                     word(honors, -2, -1), NA)) %>%
  mutate(first.honors = ifelse(str_detect(honors, "and") == TRUE &
                                 str_detect(honors, "Contract") == FALSE &
                                 str_detect(honors, "Studies") == FALSE,
                               word(honors, 1, -3), honors)) %>%
  separate(first.honors, c("first.honors.level", "first.honors.dept"), sep = " in") %>%
  mutate(first.honors.dept = ifelse(str_detect(honors, "and") == TRUE &
                                      str_detect(honors, "Contract") == FALSE &
                                      str_detect(honors, "Studies") == FALSE,
                                    gsub(" and", "", first.honors.dept), first.honors.dept)) %>%
  mutate(second.honors.dept = gsub("and ", "", second.honors.dept)) %>%
  mutate(second.honors.level = ifelse(is.na(second.honors.dept) == FALSE, first.honors.level, NA)) %>%

  select(-honors.id2, -honors.id3, -honors.id4, -honors.id5, -honors.id6, -honors.sum,
           -split.wrong2, -split.wrong3, -split.wrong4, -split.wrong5, -split.wrong10, -sw.sum, -alpha, -honors)

}

else {

  g <- file %>%
    mutate(last = sub(fixed("[,]", TRUE), "", last)) %>%
    separate(honors, c("first.honors", "second.honors"), sep = "and h") %>%
    separate(first.honors, c("first.honors.level", "first.honors.dept"), sep = " in") %>%
    separate(second.honors, c("second.honors.level", "second.honors.dept"), sep = " in") %>%
    mutate(second.honors.level = ifelse(is.na(second.honors.level) == FALSE,
                                        paste0("h", second.honors.level), second.honors.level)) %>%
    mutate(grad.year = gradyear) %>%
    select(-honors.id2, -honors.id3, -honors.id4, -honors.id5, -honors.id6, -honors.sum,
           -split.wrong2, -split.wrong3, -split.wrong4, -split.wrong5, -split.wrong10, -sw.sum, -alpha)
}


return(g)

}

clean4 <- function(file, year){

#' @title
#' @description
#' @param
#' @return
#' @usage
#' @import dplyr gender
#' @export

g <- file

gradyear <- as.numeric(paste0("20", year)) - 1

lby <- gradyear - 24
hby <- gradyear - 20

h <- gender(g$first, c(lby,  hby), method = 'ssa')

t <- gender(g$middle, c(lby, hby), method = 'ssa')

i <- left_join(g, h, by = c("first" = "name"))%>%
    select(-year_min, -proportion_male, -proportion_female, -year_max) %>%
    unique() %>%
    rename(gender.first = gender)

k <- left_join(i, t, by = c("middle" = "name"))%>%
    select(-year_min, -proportion_male, -proportion_female, -year_max) %>%
    unique() %>%
    rename(gender.middle = gender) %>%
    mutate(gender = ifelse(is.na(gender.first) == TRUE, gender.middle, gender.first)) %>%
    select(-gender.first, -gender.middle, -honors.id, -other, -split.wrong)

k <- k[, c("first", "middle", "last", "suffix", "grad.year", "gender", "pbk", "latin.honors", "first.honors.dept", "first.honors.level",
           "second.honors.dept", "second.honors.level")]

return(k)

}
