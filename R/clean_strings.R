clean1 <- function(file){

#' @title Clean Data Part One
#' @description clean1()
#' @param file
#' @return
#' @usage
#' @import stringr dplyr
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
  mutate(alphabetize = gsub(c(" ","'"), "", last)) %>%
  mutate(alphabetize = gsub("'", "", alphabetize)) %>%
  mutate(alphabetize = tolower(alphabetize))

r <- as.numeric(which(str_detect(g$first, "Bachelor") == TRUE))

g$latin_honors <- ifelse(row(g) <= r[3], "Cum Laude", NA)
g$latin_honors <- ifelse(row(g) <= r[2], "Magna Cum Laude", g$latin_honors)
g$latin_honors <- ifelse(row(g) <= r[1], "Summa Cum Laude", g$latin_honors)

return(g)

}

clean2 <- function(file){

  #' @title
  #' @description
  #' @param
  #' @return
  #' @usage
  #' @import dplyr
  #' @export

  i <- file %>%
    mutate(honors_id = lead(alphabetize, 1, default = "0") <= lag(alphabetize, 1, default = "0")) %>%
    mutate(honors_id2 = lead(alphabetize, 2, default = "0") <= lag(alphabetize, 2, default = "0")) %>%
    mutate(honors_id3 = lead(alphabetize, 3, default = "0") <= lag(alphabetize, 3, default = "0")) %>%
    mutate(honors_id4 = lead(alphabetize, 4, default = "0") <= lag(alphabetize, 4, default = "0")) %>%
    mutate(honors_id5 = lead(alphabetize, 5, default = "0") <= lag(alphabetize, 5, default = "0")) %>%
    mutate(honors_id6 = lead(alphabetize, 10, default = "0") <= lag(alphabetize, 10, default = "0")) %>%
    mutate(honors_sum = honors_id + honors_id2 + honors_id3 + honors_id4 + honors_id5 + honors_id6) %>%
    mutate(split_wrong = alphabetize < lag(alphabetize, default = "0") | alphabetize > lead(alphabetize, default = "0")) %>%
    mutate(split_wrong2 = alphabetize < lag(alphabetize, 2, default = "0") | alphabetize > lead(alphabetize, 2, default = "0")) %>%
    mutate(split_wrong3 = alphabetize < lag(alphabetize, 3, default = "0") | alphabetize > lead(alphabetize, 3, default = "0")) %>%
    mutate(split_wrong4 = alphabetize < lag(alphabetize, 4, default = "0") | alphabetize > lead(alphabetize, 4, default = "0")) %>%
    mutate(split_wrong5 = alphabetize < lag(alphabetize, 5, default = "0") | alphabetize > lead(alphabetize, 5, default = "0")) %>%
    mutate(split_wrong10 = alphabetize < lag(alphabetize, 10, default = "0") | alphabetize > lead(alphabetize, 10, default = "0")) %>%
    mutate(sw_sum = split_wrong + split_wrong2 + split_wrong3 + split_wrong4 + split_wrong5 +split_wrong10) %>%
    mutate(y = (sw_sum >3 & honors_sum <3)) %>%
    mutate(switched = (y == TRUE & first >= lag(alphabetize) & first <= lead(alphabetize) & str_count(other, " ") == 0)) %>%
    mutate(first = ifelse(switched == TRUE, paste(first, last, sep = "#"), first)) %>%
    mutate(last = ifelse(switched == TRUE, word(first, 1, sep = "#"), last)) %>%
    mutate(first = ifelse(switched == TRUE, word(first, -1, sep = "#"), first)) %>%
    mutate(other = ifelse(switched == TRUE, NA, other)) %>%
    mutate(alphabetize = gsub(c(" ","'"), "", last)) %>%
    mutate(alphabetize = gsub("'", "", alphabetize)) %>%
    mutate(alphabetize = tolower(alphabetize))

  h <- i %>%
    mutate(honors_id = lead(alphabetize, 1, default = "0") <= lag(alphabetize, 1, default = "0")) %>%
    mutate(honors_id2 = lead(alphabetize, 2, default = "0") <= lag(alphabetize, 2, default = "0")) %>%
    mutate(honors_id3 = lead(alphabetize, 3, default = "0") <= lag(alphabetize, 3, default = "0")) %>%
    mutate(honors_id4 = lead(alphabetize, 4, default = "0") <= lag(alphabetize, 4, default = "0")) %>%
    mutate(honors_id5 = lead(alphabetize, 5, default = "0") <= lag(alphabetize, 5, default = "0")) %>%
    mutate(honors_id6 = lead(alphabetize, 10, default = "0") <= lag(alphabetize, 10, default = "0")) %>%
    mutate(honors_sum = honors_id + honors_id2 + honors_id3 + honors_id4 + honors_id5 + honors_id6) %>%
    mutate(split_wrong = alphabetize < lag(alphabetize, default = "0") | alphabetize > lead(alphabetize, default = "0")) %>%
    mutate(split_wrong2 = alphabetize < lag(alphabetize, 2, default = "0") | alphabetize > lead(alphabetize, 2, default = "0")) %>%
    mutate(split_wrong3 = alphabetize < lag(alphabetize, 3, default = "0") | alphabetize > lead(alphabetize, 3, default = "0")) %>%
    mutate(split_wrong4 = alphabetize < lag(alphabetize, 4, default = "0") | alphabetize > lead(alphabetize, 4, default = "0")) %>%
    mutate(split_wrong5 = alphabetize < lag(alphabetize, 5, default = "0") | alphabetize > lead(alphabetize, 5, default = "0")) %>%
    mutate(split_wrong10 = alphabetize < lag(alphabetize, 10, default = "0") | alphabetize > lead(alphabetize, 10, default = "0")) %>%
    mutate(sw_sum = split_wrong + split_wrong2 + split_wrong3 + split_wrong4 + split_wrong5 +split_wrong10) %>%
    mutate(y = (sw_sum >3 & honors_sum <3)) %>%
    mutate(last = ifelse(y == TRUE, word(other,-2, -1), last)) %>%
    mutate(middle = ifelse(y == TRUE, word(other, 1, -3), middle)) %>%
    mutate(alphabetize = gsub(c(" ","'"), "", last)) %>%
    mutate(alphabetize = gsub("'", "", alphabetize)) %>%
    mutate(alphabetize = tolower(alphabetize))

  g <- h %>%
    mutate(honors_id = lead(alphabetize, 1, default = "0") <= lag(alphabetize, 1, default = "0")) %>%
    mutate(honors_id2 = lead(alphabetize, 2, default = "0") <= lag(alphabetize, 2, default = "0")) %>%
    mutate(honors_id3 = lead(alphabetize, 3, default = "0") <= lag(alphabetize, 3, default = "0")) %>%
    mutate(honors_id4 = lead(alphabetize, 4, default = "0") <= lag(alphabetize, 4, default = "0")) %>%
    mutate(honors_id5 = lead(alphabetize, 5, default = "0") <= lag(alphabetize, 5, default = "0")) %>%
    mutate(honors_id6 = lead(alphabetize, 10, default = "0") <= lag(alphabetize, 10, default = "0")) %>%
    mutate(honors_sum = honors_id + honors_id2 + honors_id3 + honors_id4 + honors_id5 + honors_id6) %>%
    mutate(split_wrong = alphabetize < lag(alphabetize, default = "0") | alphabetize > lead(alphabetize, default = "0")) %>%
    mutate(split_wrong2 = alphabetize < lag(alphabetize, 2, default = "0") | alphabetize > lead(alphabetize, 2, default = "0")) %>%
    mutate(split_wrong3 = alphabetize < lag(alphabetize, 3, default = "0") | alphabetize > lead(alphabetize, 3, default = "0")) %>%
    mutate(split_wrong4 = alphabetize < lag(alphabetize, 4, default = "0") | alphabetize > lead(alphabetize, 4, default = "0")) %>%
    mutate(split_wrong5 = alphabetize < lag(alphabetize, 5, default = "0") | alphabetize > lead(alphabetize, 5, default = "0")) %>%
    mutate(split_wrong10 = alphabetize < lag(alphabetize, 10, default = "0") | alphabetize > lead(alphabetize, 10, default = "0")) %>%
    mutate(sw_sum = split_wrong + split_wrong2 + split_wrong3 + split_wrong4 + split_wrong5 +split_wrong10) %>%
    mutate(y = (sw_sum >3 & honors_sum <3)) %>%
    mutate(last = ifelse(y == TRUE, word(other,-3, -1), last)) %>%
    mutate(middle = ifelse(y == TRUE, word(other, 1, -4), middle)) %>%
    mutate(alphabetize = gsub(c(" ","'"), "", last)) %>%
    mutate(alphabetize = gsub("'", "", alphabetize)) %>%
    mutate(alphabetize = tolower(alphabetize))

  f <- g %>%
    mutate(honors_id = lead(alphabetize, 1, default = "0") <= lag(alphabetize, 1, default = "0")) %>%
    mutate(honors_id2 = lead(alphabetize, 2, default = "0") <= lag(alphabetize, 2, default = "0")) %>%
    mutate(honors_id3 = lead(alphabetize, 3, default = "0") <= lag(alphabetize, 3, default = "0")) %>%
    mutate(honors_id4 = lead(alphabetize, 4, default = "0") <= lag(alphabetize, 4, default = "0")) %>%
    mutate(honors_id5 = lead(alphabetize, 5, default = "0") <= lag(alphabetize, 5, default = "0")) %>%
    mutate(honors_id6 = lead(alphabetize, 10, default = "0") <= lag(alphabetize, 10, default = "0")) %>%
    mutate(honors_sum = honors_id + honors_id2 + honors_id3 + honors_id4 + honors_id5 + honors_id6) %>%
    mutate(split_wrong = alphabetize < lag(alphabetize, default = "0") | alphabetize > lead(alphabetize, default = "0")) %>%
    mutate(split_wrong2 = alphabetize < lag(alphabetize, 2, default = "0") | alphabetize > lead(alphabetize, 2, default = "0")) %>%
    mutate(split_wrong3 = alphabetize < lag(alphabetize, 3, default = "0") | alphabetize > lead(alphabetize, 3, default = "0")) %>%
    mutate(split_wrong4 = alphabetize < lag(alphabetize, 4, default = "0") | alphabetize > lead(alphabetize, 4, default = "0")) %>%
    mutate(split_wrong5 = alphabetize < lag(alphabetize, 5, default = "0") | alphabetize > lead(alphabetize, 5, default = "0")) %>%
    mutate(split_wrong10 = alphabetize < lag(alphabetize, 10, default = "0") | alphabetize > lead(alphabetize, 10, default = "0")) %>%
    mutate(sw_sum = split_wrong + split_wrong2 + split_wrong3 + split_wrong4 + split_wrong5 +split_wrong10) %>%
    mutate(y = (sw_sum >3 & honors_sum <3)) %>%
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
#' @import dplyr
#' @export

gradyear <- as.numeric(paste0("20", year)) - 1

g <- file %>%
  mutate(last = sub(fixed("[,]", TRUE), "", last)) %>%
  separate(honors, c("first_honors", "second_honors"), sep = "and h") %>%
  separate(first_honors, c("first_honors_level", "first_honors_dept"), sep = " in") %>%
  separate(second_honors, c("second_honors_level", "second_honors_dept"), sep = " in") %>%
  mutate(second_honors_level = ifelse(is.na(second_honors_level) == FALSE,
                                        paste0("h", second_honors_level), second_honors_level)) %>%
  mutate(grad.year = gradyear) %>%
  select(-honors_id2, -honors_id3, -honors_id4, -honors_id5, -honors_id6, -honors_sum,
           -split_wrong2, -split_wrong3, -split_wrong4, -split_wrong5, -split_wrong10, -sw_sum, -alphabetize)

return(g)

}

clean4 <- function(file, year){

#' @title
#' @description
#' @param
#' @return
#' @usage
#' @import dplyr
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
    rename(gender_first = gender)

k <- left_join(i, t, by = c("middle" = "name"))%>%
    select(-year_min, -proportion_male, -proportion_female, -year_max) %>%
    unique() %>%
    rename(gender_middle = gender) %>%
    mutate(gender = ifelse(is.na(gender_first) == TRUE, gender_middle, gender_first)) %>%
    select(-gender_first, -gender_middle, -honors_id, -other, -split_wrong)

k <- k[, c("first", "middle", "last", "suffix", "grad.year", "gender", "pbk", "latin_honors", "first_honors_dept", "first_honors_level",
             "second_honors_level", "second_honors_dept")]

return(k)

}
