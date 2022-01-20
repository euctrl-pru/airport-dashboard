
share_market <- filter(TFC_MKT_DF, AIRPORT==Airport) %>%
  filter(YEAR == Last_complete_year) %>%
  group_by(RULE_NAME) %>%
  summarise(
    FLT_TOT = sum(NB_FLIGHT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    FLIGHT_TOT   = sum(FLT_TOT, na.rm = TRUE),
    FLIGHT_SHARE = round(FLT_TOT / FLIGHT_TOT, 2),
    label = paste0(RULE_NAME, "\n", FLIGHT_SHARE*100, "%"))

if (nrow(share_market)>1) {
  share_market_fig=ggplot(share_market, aes(area = FLT_TOT, fill = RULE_NAME, label = label)) +
    geom_treemap() +
    geom_treemap_text(colour = "white",
                      place = "centre",
                      size = 15,
                      grow = TRUE) +
    theme(legend.position = "none")
} else {
  share_market_fig = ggplot(data=share_market, aes(x=3, y=FLT_TOT, fill=RULE_NAME)) +
    geom_col() +
    scale_fill_brewer(palette=4) +
    coord_polar(theta="y") +
    geom_text(aes(x=3.4, label=label), position = position_stack(vjust = 0.5), size=60) +
    xlim(c(0.2, 3.5)) +
    theme_void() +
    theme(legend.position = "none")
}

ggsave(here("R", "Factsheet", "Figures", paste0("Market_segments_", Airport, ".png")), 
       width = Traffic_layout1[3]*Page_width, height = Traffic_height1, units = "cm", dpi=100, limitsize = FALSE)
