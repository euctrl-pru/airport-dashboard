
market_max_year <- params$market %>%
  pull(YEAR) %>%
  max() %>%
  unique()

share_market <- params$market %>%
  group_by(RULE_NAME, YEAR) %>%
  summarise(
    FLT_TOT = sum(NB_FLIGHT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(YEAR) %>% 
  mutate(
    FLIGHT_TOT   = sum(FLT_TOT, na.rm = TRUE),
    FLIGHT_SHARE = round(FLT_TOT / FLIGHT_TOT, 2),
    LABELS       = paste0(RULE_NAME, ": ", FLIGHT_SHARE*100, "%"),
    label        = paste0(RULE_NAME, "\n", FLIGHT_SHARE*100, "%")
  ) 
# %>%
# filter(FLIGHT_SHARE > 0)

filter_years <- share_market %>%
  pull(YEAR) %>%
  unique() %>% 
  sort()

# center_text <- paste0("<b>", market_max_year, "</b>")

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

share_market %>%
  plot_ly(
    labels        = ~RULE_NAME,
    values        = ~FLIGHT_SHARE,
    text          = ~label,
    textinfo      = 'text',
    # sort          = FALSE,
    name          = "",
    hovertemplate = ~paste0(RULE_NAME, ': ', FLT_TOT, ' (', FLIGHT_SHARE*100, '%)'),
    # marker        = list(colors = ~Colours),
    customdata    = ~YEAR,
    transforms    = list(
      list(
        type        = "filter",
        target      = "customdata",
        operation   = "=",
        value       = market_max_year
      )
    )) %>%
  add_pie(
    hole         = 0.4,
    textposition = "outside"
  ) %>%
  layout(
    showlegend  = TRUE,
    updatemenus = list(button_type_list)) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  )





# Factsheet figure

if (nrow(share_market)>1) {
  share_market_fig=ggplot(share_market, aes(area = FLT_TOT, fill = RULE_NAME, label = label)) +
    geom_treemap() +
    geom_treemap_text(colour = "white",
                      place = "centre",
                      size = 15,
                      grow = TRUE) +
    theme_factsheet() +
    theme(legend.position = "none")
} else {
  share_market_fig = ggplot(data=share_market, aes(x=3, y=FLT_TOT, fill=RULE_NAME)) +
    geom_col() +
    scale_fill_brewer(palette=4) +
    coord_polar(theta="y") +
    geom_text(aes(x=3.4, label=label), position = position_stack(vjust = 0.5), size=60) +
    xlim(c(0.2, 3.5)) +
    theme_factsheet() +
    theme_void() +
    theme(legend.position = "none")
}

ggsave(here("media", "factsheet", paste0("Market_segments_", params$icao, ".png")), 
       width = Traffic_layout1[3]*Page_width, height = Traffic_height1, units = "cm", dpi=100, limitsize = FALSE)

