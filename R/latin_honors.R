a <- all %>%
  select(grad_year, latin_honors) %>%
  group_by(grad_year, latin_honors) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(grad_year) %>%
  mutate(yrly_count = sum(count)) %>%
  filter(is.na(latin_honors) == FALSE) %>%
  mutate(yrly_pct = count/yrly_count)

 # %>%
 #  mutate(yrly_pct = ifelse(is.na(latin_honors) == TRUE, 1 - yrly_pct, yrly_pct)) %>%
 #  mutate(latin_honors = ifelse(is.na(latin_honors) == TRUE, "Total Honors", latin_honors))
 #


a %>%
  ggplot(aes(grad_year, yrly_pct, fill = latin_honors)) +
     geom_bar(stat = "identity", position = "dodge")


b <- a %>%
  mutate(yrly_pct = ifelse(latin_honors == "Magna Cum Laude", yrly_pct + lead(yrly_pct), yrly_pct)) %>%
  mutate(yrly_pct = ifelse(latin_honors == "Cum Laude", yrly_pct + lead(yrly_pct), yrly_pct))

b %>%
  ggplot(aes(grad_year, yrly_pct, fill = latin_honors)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = c(.02, .15, .35))

