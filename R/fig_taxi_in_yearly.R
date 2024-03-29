
TXIN_YY <- params$txin_yy %>%
  tidyr::pivot_longer(
    cols      = c("AVG_UNIMP_TIME", "AVG_ADD_TIME"),
    names_to  = "TYPE",
    values_to = "TIME"
  )

TXIN_YY <- TXIN_YY %>%
  mutate(TYPE = factor(
    TYPE,
    levels = c("AVG_UNIMP_TIME", "AVG_ADD_TIME"),
    labels = c("Avg. Reference time", "Avg. Additional time")
  ))

TXIN_YY <- TXIN_YY %>%
  mutate(YEAR = as.character(YEAR))



########################## KEY MESSAGE #########################################

TXIN_VAR <- params$txin_mm %>%
  filter(AVG_ADD_TIME != "NaN") %>%
  select(YEAR, TOT_FLT, TOT_ADD_TIME) %>%
  group_by(YEAR) %>%
  summarise(
    TOT_ADD_TIME = sum(TOT_ADD_TIME, na.rm = TRUE),
    TOT_FLT = sum(TOT_FLT, na.rm = TRUE)
  ) %>%
  ungroup()

CUM_TXIN <- params$txin_mm %>%
  filter(AVG_ADD_TIME != "NaN") %>%
  select(YEAR, MONTH_NUM, TOT_ADD_TIME, TOT_FLT) %>%
  mutate(across(c(YEAR, MONTH_NUM), as.numeric)) %>%
  filter(YEAR %in% c(max(YEAR), max(YEAR) - 1))

YEAR_MAX_MONTH <- CUM_TXIN %>%
  filter(YEAR == max(YEAR)) %>%
  pull(MONTH_NUM) %>%
  max() %>%
  unique()

CUM_TXIN <- CUM_TXIN %>%
  filter(MONTH_NUM <= YEAR_MAX_MONTH) %>%
  group_by(YEAR) %>%
  summarise(TOT_TXIN = sum(TOT_ADD_TIME, na.rm = TRUE) / sum(TOT_FLT, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(CHANGE = (TOT_TXIN - lag(TOT_TXIN)) / lag(TOT_TXIN))

msg_tmpl <- "<b>Add.Change Jan-{mnt}<br>{yr2} vs {yr1}:<br>{pct}% </b>"

msg <- str_glue(msg_tmpl,
                mnt = pick_mth[YEAR_MAX_MONTH],
                yr1 = CUM_TXIN$YEAR[1],
                yr2 = CUM_TXIN$YEAR[2],
                pct = round(100 * CUM_TXIN[2, "CHANGE"], 2)
)

# msg <- paste0("<b>Change Jan-",
#               pick_mth[YEAR_MAX_MONTH],
#               "<br>",
#               CUM_TXIN$YEAR[2],
#               " vs ",
#               CUM_TXIN$YEAR[1],
#               ":<br>",
#               round(100* CUM_TXIN[2,"CHANGE"], 2),"% </b>")

ALL_YEAR <- TXIN_YY %>%
  filter(TIME != "NaN") %>%
  pull(YEAR) %>%
  unique()

msg_y <- params$txin_yy %>%
  filter(AVG_ADD_TIME != "NaN") %>%
  filter(YEAR == as.integer(max(YEAR)) - 1) %>%
  mutate(TOT_TXIN = 1.1 * (AVG_UNIMP_TIME + AVG_ADD_TIME)) %>%
  pull(TOT_TXIN)

key_msg <- list(
  x = length(ALL_YEAR) - 1,
  y = msg_y,
  text = msg,
  size = 14,
  showarrow = FALSE
)

################################################################################


TXIN_YY %>%
  plotly::plot_ly(
    x          = ~YEAR,
    y          = ~TIME,
    type       = "bar",
    showlegend = TRUE,
    color      = ~TYPE
  ) %>%
  layout(
    barmode   = "stack",
    hovermode = "x unified",
    xaxis     = list(title = ""),
    yaxis     = list(
      title       = "Average Taxi-in time [min/arr]",
      titlefont   = list(size = 11),
      hoverformat = ".2f"
    ),
    annotations = key_msg
  ) %>%
  # add_annotations(x         = 1, 
  #                 y         = -0.1, 
  #                 text      = "Source: APDF", 
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

TXIN_YY_fig = ggplot(data=TXIN_YY) +
  geom_bar(aes(x=YEAR, y=TIME, fill=TYPE), stat="identity", position = position_stack(reverse = TRUE)) +
  # scale_fill_manual(values=slot_col) +
  theme_factsheet() +
  # theme_bw() +
  # theme(plot.title = element_blank(),
  #       legend.title=element_blank(),
  #       legend.text=element_text(size=36),
  #       legend.position = "bottom",
  #       axis.text=element_text(size=100),
  #       axis.title=element_text(size=100),
  #       axis.title.x = element_blank(),
  #       plot.margin = unit(c(5.5, 20, 5.5, 60), "pt")) +
  labs(x="", y="Average Taxi-In Time (min/arr)\n")
ggsave(here("media", "factsheet", paste0("Taxi_In_Times_Yearly_", params$icao, ".pdf")), plot=TXIN_YY_fig, 
       width = Taxi_in_times_layout1[1]*Page_width, height = Taxi_in_times_height1, units = "cm", dpi=100, limitsize = FALSE)

