# all <- all %>%
#   rename(grad.year = grad_year)
#
# s <- all %>%
#   select(grad.year, pbk) %>%
#   group_by(grad.year, pbk) %>%
#   summarize(count = n()) %>%
#   ungroup() %>%
#   group_by(grad.year) %>%
#   mutate(yrly.count = sum(count)) %>%
#   filter(pbk == TRUE) %>%
#   mutate(yrly.pct = count/yrly.count)
#
# s %>%
#   ggplot(aes(grad.year, yrly.pct)) +
#     geom_bar(stat = "identity")
