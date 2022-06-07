
market_max_year <- params$market %>%
  pull(YEAR) %>%
  max() %>%
  unique()

share_market <- params$market %>%
  filter(YEAR == market_max_year) %>%
  group_by(RULE_NAME) %>%
  summarise(
    FLT_TOT = sum(NB_FLIGHT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    FLIGHT_TOT   = sum(FLT_TOT, na.rm = TRUE),
    FLIGHT_SHARE = round(FLT_TOT / FLIGHT_TOT, 2),
    LABELS       = paste0(RULE_NAME, "<br>", FLIGHT_SHARE, "%"),
    label        = paste0(RULE_NAME, "\n", FLIGHT_SHARE*100, "%")
  ) 
# %>%
# filter(FLIGHT_SHARE > 0)

center_text <- paste0("<b>", market_max_year, "</b>")

share_market %>%
  plot_ly(
    labels = ~RULE_NAME,
    values = ~FLIGHT_SHARE
  ) %>%
  add_pie(
    hole         = 0.4,
    textposition = "outside"
  ) %>%
  add_annotations(
    text      = center_text,
    font      = list(size = 18),
    x         = 0.5,
    y         = 0.5,
    showarrow = FALSE
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  )





# Factsheet figure

share_market_fac <- params$market %>%
  filter(YEAR == Last_complete_year) %>%
  group_by(RULE_NAME) %>%
  summarise(
    FLT_TOT = sum(NB_FLIGHT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    FLIGHT_TOT   = sum(FLT_TOT, na.rm = TRUE),
    text_y = cumsum(FLT_TOT) - FLT_TOT/2,
    FLIGHT_SHARE = round(FLT_TOT / FLIGHT_TOT, 2),
    label        = paste0(RULE_NAME, "\n", FLIGHT_SHARE*100, "%")
  ) 

# if (nrow(share_market)>1) {
#   share_market_fig=ggplot(share_market, aes(area = FLT_TOT, fill = RULE_NAME, label = label)) +
#     geom_treemap() +
#     geom_treemap_text(colour = "white",
#                       place = "centre",
#                       size = 15,
#                       grow = TRUE) +
#     theme_factsheet() +
#     theme(legend.position = "none")
# } else {
  share_market_fig = ggplot(data=share_market_fac, aes(x=3, y=FLT_TOT, fill=rev(RULE_NAME))) +
    geom_bar(stat = "identity") +
    # scale_fill_brewer(palette=4) +
    coord_polar(theta="y") +
    geom_label_repel(aes(y=text_y, label=label), size=40, nudge_x = 1) +
    xlim(c(1.5, 4.5)) +
    theme_factsheet() +
    theme_void() +
    theme(legend.position = "none") +
    annotate(geom = 'text', x = 1.5, y = 0, label = Last_complete_year, size = 40)
# }

ggsave(here("media", "factsheet", paste0("Market_segments_", params$icao, ".png")), 
       width = Traffic_layout1[3]*Page_width, height = Traffic_height1, units = "cm", dpi=100, limitsize = FALSE)

