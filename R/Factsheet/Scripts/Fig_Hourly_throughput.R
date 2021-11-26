
thru <- filter(THRU_DF, AIRPORT=="EBBR") %>%
  filter(
    TIME >= hm("05:55"),
    TIME <= hm("23:05")
  ) %>%
  mutate(PHASE = factor(PHASE,
                        levels = c("DEP", "ARR"),
                        labels = c("Departures", "Arrivals")))

thru_tot <- thru %>%
  group_by(TIME) %>%
  summarise(ROLLING_HOUR_MVT = sum(ROLLING_HOUR_MVT, na.rm = TRUE)) %>%
  mutate(PHASE = "Total") %>%
  ungroup()

thru <- thru %>%
  bind_rows(thru_tot)

thru_fig=ggplot(data=thru) +
  geom_line(aes(x = TIME, y = ROLLING_HOUR_MVT, color = PHASE))
ggsave(here("R", "Factsheet", "Figures", paste0("Hourly_throughput_", Airport, ".png")), 
       width = Traffic_layout1[3]*Page_width, height = Traffic_height1, units = "cm", dpi=100, limitsize = FALSE)
