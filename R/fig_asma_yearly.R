
ASMA_YY <- params$asma_yy %>%
  tidyr::pivot_longer(
    cols = c("AVG_UNIMP_TIME", "AVG_ADD_TIME"),
    names_to = "TYPE",
    values_to = "TIME"
  )

ASMA_YY <- ASMA_YY %>%
  mutate(TYPE = factor(TYPE,
                       levels = c("AVG_UNIMP_TIME", "AVG_ADD_TIME"),
                       labels = c("Avg. Reference time", "Avg. Additional time")
  ))
ASMA_YY <- ASMA_YY %>%
  mutate(YEAR = as.character(YEAR))

########################## KEY MESSAGE #########################################

ASMA_VAR <- params$asma_mm %>%
  filter(AVG_ADD_TIME != "NaN") %>%
  select(YEAR, TOT_FLT, TOT_ADD_TIME) %>%
  group_by(YEAR) %>%
  summarise(
    TOT_ADD_TIME = sum(TOT_ADD_TIME, na.rm = TRUE),
    TOT_FLT = sum(TOT_FLT, na.rm = TRUE)
  ) %>%
  ungroup()

CUM_ASMA <- params$asma_mm %>%
  filter(AVG_ADD_TIME != "NaN") %>%
  select(YEAR, MONTH_NUM, TOT_ADD_TIME, TOT_FLT) %>%
  mutate(across(c(YEAR, MONTH_NUM), as.numeric)) %>%
  filter(YEAR %in% c(max(YEAR), max(YEAR) - 1))

YEAR_MAX_MONTH <- CUM_ASMA %>%
  filter(YEAR == max(YEAR)) %>%
  pull(MONTH_NUM) %>%
  max() %>%
  unique()

CUM_ASMA <- CUM_ASMA %>%
  filter(MONTH_NUM <= YEAR_MAX_MONTH) %>%
  group_by(YEAR) %>%
  summarise(TOT_ASMA = sum(TOT_ADD_TIME, na.rm = TRUE) / sum(TOT_FLT, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(CHANGE = (TOT_ASMA - lag(TOT_ASMA)) / lag(TOT_ASMA))

msg_tmpl <- "<b>Add.Change Jan-{mnt}<br>{yr2} vs {yr1}:<br>{pct}% </b>"

msg <- str_glue(msg_tmpl,
                mnt = pick_mth[YEAR_MAX_MONTH],
                yr1 = CUM_ASMA$YEAR[1],
                yr2 = CUM_ASMA$YEAR[2],
                pct = round(100 * CUM_ASMA[2, "CHANGE"], 2)
)

ALL_YEAR <- ASMA_YY %>%
  filter(TIME != "NaN") %>%
  pull(YEAR) %>%
  unique()

msg_y <- params$asma_yy %>%
  filter(AVG_ADD_TIME != "NaN") %>%
  filter(YEAR == as.integer(max(YEAR)) - 1) %>%
  mutate(TOT_ASMA = 1.1 * (AVG_UNIMP_TIME + AVG_ADD_TIME)) %>%
  pull(TOT_ASMA)

key_msg <- list(
  x = length(ALL_YEAR) - 1,
  y = msg_y,
  text = msg,
  size = 14,
  showarrow = FALSE
)

###########################################################################

if (nrow(ASMA_YY)>0) {
  
  
ASMA_YY %>%
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
      title       = "Average ASMA time [min/arr]",
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
) %>% 
  add_download_button(
    ASMA_YY %>% 
      select(
        AIRPORT, 
        YEAR, 
        TOT_REF_TIME = TOT_UNIMP_TIME,
        TOT_ADD_TIME, 
        TOT_FLT,  
        TYPE, 
        AVG_ASMA_TIME = TIME), 
    "ASMA_YY")

} else {
  
  cat("<center> 
No data available </center>")
  
}



# Factsheet figure

ASMA_YY_fig = ggplot(data=ASMA_YY) +
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
  labs(x="", y="Average ASMA time (min/arr)\n")
ggsave(here("media", "factsheet", paste0("ASMA_Yearly_", params$icao, ".pdf")), plot=ASMA_YY_fig, 
       width = ASMA_layout1[1]*Page_width, height = ASMA_height1, units = "cm", dpi=100, limitsize = FALSE)

