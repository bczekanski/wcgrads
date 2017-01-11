f <- w %>%
  rename(strings = value) %>%
  mutate(pbk = str_detect(strings, fixed("*", TRUE))) %>%
  mutate(plus_sign = str_detect(strings, fixed("+", TRUE)))
f <- as.data.frame(f, stringsasfactors = FALSE)

y <- as.numeric(which(f$strings == "Bachelor of Arts, Magna Cum Laude"))
k <- as.numeric(which(f$strings == "Bachelor of Arts, Cum Laude"))
z <- as.numeric(which(f$strings == "Bachelor of Arts"))

f$latin_honors <- ifelse(row(f) <= z, "Cum Laude", NA)
f$latin_honors <- ifelse(row(f) <= k, "Summa Cum Laude", f$latin_honors)
f$latin_honors <- ifelse(row(f) <= y, "Magna Cum Laude", f$latin_honors)

g <- f %>%

  separate(strings, c("name", "honors"), sep = ", with") %>%
  separate(name, c("first", "second"), sep = " ", extra = "merge") %>%
  separate(second, c("second", "third"), sep = " ", fill = "left") %>%
  mutate(first = sub(fixed("[*]", TRUE), "", first)) %>%
  mutate(first = sub(fixed("[+]", TRUE), "", first)) %>%
  separate(honors, c("first_honors", "second_honors"), sep = "and with") %>%
  separate(first_honors, c("first_honors_level", "first_honors_dept"), sep = " in") %>%
  separate(second_honors, c("second_honors_level", "second_honors_dept"), sep = " in")

gradyear <- as.numeric(paste0("20", file))

g <- g %>%
  mutate(grad_year = gradyear)


lby <- gradyear - 24
hby <- gradyear - 20
h <- gender(g$first, c(lby,  hby), method = 'ssa')

i <- left_join(g, h, by = c("first" = "name"))%>%
  select(-year_min, -proportion_male, -proportion_female, -year_max) %>%
  unique()
