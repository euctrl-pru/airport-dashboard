
mvts_pa <- params$tfc %>% # filter(TFC_DF, AIRPORT=="EBBR")
  select(YEAR, FLT_TOT) %>%
  group_by(YEAR) %>%
  summarise(FLT_TOT = sum(FLT_TOT, na.rm = TRUE)) %>%
  ungroup()

cum_mvts <- params$tfc %>%
  select(YEAR, MONTH_NUM, FLT_DATE, FLT_TOT) %>%
  mutate(across(c(YEAR, MONTH_NUM), as.numeric)) %>%
  filter(YEAR %in% c(max(YEAR), max(YEAR) - 1))

this_year_max_month <- cum_mvts %>%
  filter(YEAR == max(YEAR)) %>%
  pull(MONTH_NUM) %>%
  max() %>%
  unique()

cum_mvts <- cum_mvts %>%
  filter(MONTH_NUM <= this_year_max_month) %>%
  group_by(YEAR) %>%
  summarise(FLT_TOT = sum(FLT_TOT, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(CHANGE = (FLT_TOT - lag(FLT_TOT)) / lag(FLT_TOT))

msg_tmpl <- "<b>Change Jan-{mnt}<br>{yr2} vs {yr1}:<br>{pct}% </b>"

msg <- str_glue(msg_tmpl,
                mnt = pick_mth[this_year_max_month],
                yr1 = cum_mvts$YEAR[1],
                yr2 = cum_mvts$YEAR[2],
                pct = round(100 * cum_mvts[2, "CHANGE"], 2)
)

ALL_YEAR <- mvts_pa %>%
  pull(YEAR) %>%
  unique()

msg_y <- mvts_pa %>%
  filter(YEAR == as.integer(max(YEAR)) - 1) %>%
  mutate(FLT_TOT = 0.7 * FLT_TOT) %>%
  pull(FLT_TOT)

key_msg <- list(
  x = length(ALL_YEAR) - 1,
  y = msg_y,
  text = msg,
  size = 14,
  showarrow = FALSE
)

xax <- list(title = "")

yax <- list(
  title = "Number of flights",
  titlefont = list(size = 11)
)

mvts_pa <- mvts_pa %>%
  mutate(YEAR = as.character(YEAR))

mvts_pa %>%
  plot_ly(
    x = ~YEAR,
    y = ~FLT_TOT
  ) %>%
  add_trace(
    type = "bar",
    name = "Traffic",
    hoverinfo = "text",
    hovertemplate = paste("Year: %{x}", "<br>Flights: %{y:.r}")
  ) %>%
  layout(
    xaxis = xax,
    yaxis = yax,
    annotations = key_msg
  ) %>%
  # add_annotations(x         = 1, 
  #                 y         = -0.1, 
  #                 text      = "Source: Network Manager", 
  #                 showarrow = F, 
  #                 xref      = 'paper', 
  #                 yref      = 'paper', 
  #                 xanchor   = 'right', 
  #                 yanchor   = 'auto', 
  #                 xshift    = 0, 
  #                 yshift    = -10,
  #                 font      = list(size=12)
# ) %>%
config(
  displaylogo = FALSE,
  modeBarButtonsToRemove = config_bar_remove_buttons
)




# Factsheet figure

mvts_pa_fig = ggplot(data=mvts_pa) +
  geom_bar(aes(x=YEAR, y=FLT_TOT/1000), position = "dodge", stat="identity") +
  theme_factsheet() +
  labs(x="", y="Total movements\n") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(labels = label_number(suffix = "k"))
ggsave(here("media", "factsheet", paste0("Annual_mvmts_", params$icao, ".png")), plot=mvts_pa_fig, 
       width = Traffic_layout1[1]*Page_width, height = Traffic_height1, units = "cm", dpi=100, limitsize = FALSE)
