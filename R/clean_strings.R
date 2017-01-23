clean1 <- function(file){

#' @title Clean Data Part One
#' @description clean1() is the first part of the data cleaning series
#' @param file A file that is the output of makestrings(), a set of complete strings,
#' each a person in the Williams College Course Catalog.
#' @return clean1 returns partially cleaned data ready for clean2
#' @usage clean1("file")
#' @import stringr dplyr tidyr
#' @export

g <- file %>%
  rename(strings = value) %>%
  #Convert text encoding
  mutate(strings = iconv(strings, from = "ISO-8859-1", to = "UTF-8")) %>%
  #Cut out unnecessary numbers and weird characters
  mutate(strings = gsub('[0-9]', "", strings)) %>%
  mutate(strings = gsub(" \f", "", strings)) %>%
  #Detect asterisk indicating Phi Beta kappa
  mutate(pbk = str_detect(strings, fixed("*", TRUE))) %>%
  #Break into name section and honors section
  separate(strings, c("name", "honors"), sep = ", with") %>%
  #In case where name is one string with no spaces, break by hyphens
  mutate(name = ifelse(str_count(name, " ") == 0, gsub(fixed("-", TRUE), fixed(" ", TRUE), name), name)) %>%
  #Identify first name
  separate(name, c("first", "other"), sep = " ", extra = "merge") %>%
  #Remove unnecessary characters
  mutate(first = sub(fixed("[*]", TRUE), "", first)) %>%
  mutate(first = sub(fixed("[+]", TRUE), "", first)) %>%
  #Identify last name
  mutate(last = word(other, -1)) %>%
  #Identify suffix and correct middle and lastname if necessary
  mutate(suffix = ifelse(last == "I" | last == "II" | last == "III" | last == "IV" | last == "V" | last == "Jr.", last, NA)) %>%
  mutate(last = ifelse(is.na(suffix) == FALSE, word(other, -2), last)) %>%
  mutate(middle = ifelse(is.na(suffix) == FALSE, word(other, 1, -3), word(other, 1, -2))) %>%
  #Create alpha variable to use to check alphabetization
  mutate(alpha = gsub(c(" ","'"), "", last)) %>%
  mutate(alpha = gsub("'", "", alpha)) %>%
  mutate(alpha = tolower(alpha))
#Identify where different latin honors levels start and end, and assign latin honors
r <- as.numeric(which(str_detect(g$first, "Bachelor") == TRUE))

g$latin.honors <- ifelse(row(g) <= r[3], "Cum Laude", NA)
g$latin.honors <- ifelse(row(g) <= r[2], "Magna Cum Laude", g$latin.honors)
g$latin.honors <- ifelse(row(g) <= r[1], "Summa Cum Laude", g$latin.honors)
#Remove strings indicating changes in latin honors, as they are no longer necessary
g <- g %>%
  filter(str_detect(first, "Bachelor") == FALSE)

return(g)

}

clean2 <- function(file){

#' @title Clean Data Part Two
#' @description clean2() is the second part of the data cleaning series, it focuses on correcting last names that have spaces
#' and have been read incorrectly as a result.
#' @param file A file that is the output of clean1(), a set of partially cleaned strings,
#' each a person in the Williams College Course Catalog.
#' @return clean2 returns partially cleaned data ready for clean3
#' @usage clean2("file")
#' @import dplyr tidyr
#' @export


  i <- file %>%
    #Run tests to see if the last name fits alphabetically with the names around it
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
    #Run tests to examine if the first and last names are in the reverse of the usual order, because one is a family name, and correct it
    mutate(switched = (y == TRUE & first >= lag(alpha) & first <= lead(alpha) & str_count(other, " ") == 0)) %>%
    mutate(first = ifelse(switched == TRUE, paste(first, last, sep = "#"), first)) %>%
    mutate(last = ifelse(switched == TRUE, word(first, 1, sep = "#"), last)) %>%
    mutate(first = ifelse(switched == TRUE, word(first, -1, sep = "#"), first)) %>%
    mutate(other = ifelse(switched == TRUE, NA, other)) %>%
    mutate(alpha = gsub(c(" ","'"), "", last)) %>%
    mutate(alpha = gsub("'", "", alpha)) %>%
    mutate(alpha = tolower(alpha))
#Repeat, focusing on misread last names due to spaces
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
  #Repeat, focusing on misread last names due to spaces
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
  #Repeat, focusing on misread last names due to spaces
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

#' @title Clean Data Part Three
#' @description clean3() is the third part of the data cleaning series, it focuses on departmental honors.
#' @param file A file that is the output of clean2(), a set of partially cleaned strings,
  #' each a person in the Williams College Course Catalog.
#' @param year The year after the course catalog came out, for the 1999-2000 catalog, the year would be "00", the same as the file.
#' @return clean3 returns partially cleaned data ready for clean4
#' @usage clean3("file", "year")
#' @import dplyr tidyr
#' @export

#Define graduation year as the year the catalog came out
gradyear <- as.numeric(paste0("20", year)) - 1

if(gradyear == 2014) {

g <- file %>%
  mutate(grad.year = gradyear) %>%
  #Drop the comma still attached to some last names
  mutate(last = sub(fixed("[,]", TRUE), "", last)) %>%
  #Break up dual departmental honors into two
  separate(honors, c("first.honors", "second.honors"), sep = "and with") %>%
  #Break into department and level
  separate(first.honors, c("first.honors.level", "first.honors.dept"), sep = " in") %>%
  separate(second.honors, c("second.honors.level", "second.honors.dept"), sep = " in") %>%
  select(-honors.id2, -honors.id3, -honors.id4, -honors.id5, -honors.id6, -honors.sum,
           -split.wrong2, -split.wrong3, -split.wrong4, -split.wrong5, -split.wrong10, -sw.sum, -alpha)

}

else if(gradyear == 2005 | gradyear == 2008 | gradyear == 2013) {

g <- file %>%
  mutate(grad.year = gradyear) %>%
  mutate(last = sub(fixed("[,]", TRUE), "", last)) %>%
  #Find second honors dept by taking the last word in the honors string
  mutate(second.honors.dept = ifelse(str_detect(honors, "and") == TRUE &
                                       str_detect(honors, "Contract") == FALSE &
                                       str_detect(honors, "Studies") == FALSE,
                                     word(honors, -2, -1), NA)) %>%
  #Find the first honors section by taking all but the last two words from the honors string
  mutate(first.honors = ifelse(str_detect(honors, "and") == TRUE &
                                 str_detect(honors, "Contract") == FALSE &
                                 str_detect(honors, "Studies") == FALSE,
                               word(honors, 1, -3), honors)) %>%
  #Break first honors in to level and dept
  separate(first.honors, c("first.honors.level", "first.honors.dept"), sep = " in") %>%
  #Drop "and"s
  mutate(first.honors.dept = ifelse(str_detect(honors, "and") == TRUE &
                                      str_detect(honors, "Contract") == FALSE &
                                      str_detect(honors, "Studies") == FALSE,
                                    gsub(" and", "", first.honors.dept), first.honors.dept)) %>%
  mutate(second.honors.dept = gsub("and ", "", second.honors.dept)) %>%
  #Honors are always equal, set second honors level to first honors level
  mutate(second.honors.level = ifelse(is.na(second.honors.dept) == FALSE, first.honors.level, NA)) %>%

  select(-honors.id2, -honors.id3, -honors.id4, -honors.id5, -honors.id6, -honors.sum,
           -split.wrong2, -split.wrong3, -split.wrong4, -split.wrong5, -split.wrong10, -sw.sum, -alpha, -honors)

}

else {

  g <- file %>%
    mutate(last = sub(fixed("[,]", TRUE), "", last)) %>%
    #break honors into first and second
    separate(honors, c("first.honors", "second.honors"), sep = "and h") %>%
    #Break into dept and level
    separate(first.honors, c("first.honors.level", "first.honors.dept"), sep = " in") %>%
    separate(second.honors, c("second.honors.level", "second.honors.dept"), sep = " in") %>%
    #Add the h that was dropped earlier
    mutate(second.honors.level = ifelse(is.na(second.honors.level) == FALSE,
                                        paste0("h", second.honors.level), second.honors.level)) %>%
    mutate(grad.year = gradyear) %>%
    select(-honors.id2, -honors.id3, -honors.id4, -honors.id5, -honors.id6, -honors.sum,
           -split.wrong2, -split.wrong3, -split.wrong4, -split.wrong5, -split.wrong10, -sw.sum, -alpha)
}


return(g)

}

clean4 <- function(file, year){

#' @title Clean Data Part Four
#' @description clean4() is the fourth part of the data cleaning series, it focuses on adding gender information.
#' @param file A file that is the output of clean3(), a set of partially cleaned strings,
  #' each a person in the Williams College Course Catalog.
#' @param year The year after the course catalog came out, for the 1999-2000 catalog, the year would be "00", the same as the file
#' @return clean4 returns completely cleaned dataset for a single year.
#' @usage clean4("file", "year")
#' @import dplyr gender genderdata
#' @export

g <- file

gradyear <- as.numeric(paste0("20", year)) - 1

lby <- gradyear - 24
hby <- gradyear - 20

h <- gender(g$first, c(lby,  hby), method = 'ssa')

t <- gender(g$middle, c(lby, hby), method = 'ssa')
#Get gender of first name
i <- left_join(g, h, by = c("first" = "name"))%>%
    select(-year_min, -proportion_male, -proportion_female, -year_max) %>%
    unique() %>%
    rename(gender.first = gender)
#Get gender of middle name, if no gender predicted by first name
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
