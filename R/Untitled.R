cleanstrings <- function(file, grad.year){

i <-file %>%
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
