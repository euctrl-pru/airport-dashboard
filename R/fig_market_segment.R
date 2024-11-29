
mkt_seg=c("Lowcost", "Scheduled", "Mainline", "Regional", "Business", "Cargo", "Non-Scheduled", "Military", "Other")
colours = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "pink", "#7f7f7f", "#bcbd22")
mkt_seg_col=data.frame(cbind(mkt_seg, colours))

share_market <- params$market %>%
  group_by(YEAR, RULE_NAME) %>%
  summarise(
    FLT_TOT = sum(NB_FLIGHT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(YEAR) %>% 
  mutate(
    FLIGHT_TOT   = sum(FLT_TOT, na.rm = TRUE),
    FLIGHT_SHARE = round(FLT_TOT / FLIGHT_TOT, 2),
    RULE_NAME    = factor(RULE_NAME, levels = c("Lowcost", "Scheduled", "Mainline", "Regional", "Business", "Cargo", "Non-Scheduled", "Military", "Other")),
    LABELS       = paste0(RULE_NAME, "<br>", FLIGHT_SHARE, "%"),
    label        = paste0(RULE_NAME, "\n", FLIGHT_SHARE*100, "%")
  ) %>% 
  left_join(mkt_seg_col, by=c("RULE_NAME"="mkt_seg"))

share_market_copy <- cbind(share_market) %>% select(-colours)

filter_years <- share_market %>%
  pull(YEAR) %>%
  unique()

max_year <- max(filter_years)

button_list <- lapply(
  1:length(filter_years),
  function(x) {
    list(
      method = "restyle",
      args = list("transforms[0].value", filter_years[x]),
      label = filter_years[x]
    )
  }
)

button_type_list <- list(
  buttons   = button_list,
  type      = "buttons",
  bgcolor   = "#c3c1e8",
  active    = length(filter_years) - 1,
  direction = "right",
  xref      = "paper",
  x         = 0.3,
  xanchor   = "left",
  yref      = "paper",
  y         = 1.2,
  yanchor   = "bottom"
)

share_market_fig=share_market %>%
  plot_ly(
    labels     = ~factor(RULE_NAME, levels = c("Lowcost", "Scheduled", "Mainline", "Regional", "Business", "Cargo", "Non-Scheduled", "Military", "Other")),
    values     = ~FLIGHT_SHARE,
    sort=FALSE,
    direction = "clockwise",
    textposition = "outside",
    type='pie',
    hole=0.4,
    marker = list(colors = ~colours),
    customdata = ~YEAR,
    transforms = list(list(type      = 'filter',
                           target    = "customdata",
                           operation = '=',
                           value     = max_year))
  ) %>%
  add_pie(
    hole         = 0.4
  ) %>%
  layout(showlegend  = TRUE,
         updatemenus = list( button_type_list )) %>% 
  config(
    displaylogo = FALSE#,
    #modeBarButtonsToRemove = config_bar_remove_buttons
  ) %>% 
  add_download_button(share_market_copy %>% 
    select(YEAR, MARKET_SEGMENT = RULE_NAME, FLT_TOT, FLT_TOT_YEAR, SHARE = FLIGHT_SHARE), 
    "SHARE_MARKET_SEGMENT_YY")

share_market_fig



# Factsheet figure

if (nrow(share_market)>1) {
  share_market_fig=ggplot(filter(share_market, YEAR==max_year), aes(area = FLT_TOT, fill = RULE_NAME, label = label)) +
    geom_treemap() +
    geom_treemap_text(colour = "white",
                      place = "centre",
                      size = 15,
                      grow = TRUE) +
    theme_factsheet() +
    theme(legend.position = "none")
} else {
  share_market_fig = ggplot(data=filter(share_market, YEAR==max_year), aes(x=3, y=FLT_TOT, fill=RULE_NAME)) +
    geom_col() +
    scale_fill_brewer(palette=4) +
    coord_polar(theta="y") +
    geom_text(aes(x=3.4, label=label), position = position_stack(vjust = 0.5), size=60) +
    xlim(c(0.2, 3.5)) +
    theme_factsheet() +
    theme_void() +
    theme(legend.position = "none")
}

ggsave(here("media", "factsheet", paste0("Market_segments_", params$icao, ".pdf")), 
       width = Traffic_layout1[3]*Page_width, height = Traffic_height1, units = "cm", dpi=100, limitsize = FALSE)

