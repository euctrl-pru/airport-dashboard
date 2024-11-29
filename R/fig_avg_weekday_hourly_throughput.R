
thru <- params$thru %>% # pack_thru(THRU_DF, "EBBR") %>% 
  # filter(
  #   TIME >= hm("05:55"),
  #   TIME <= hm("23:05")
  # ) %>%
  mutate(PHASE = factor(PHASE,
                        levels = c("DEP", "ARR", "TOT"),
                        labels = c("Departures", "Arrivals", "Total")
  ))

thru_yr <- unique(thru$YEAR)

thru_mth <- unique(thru$MONTH_NUM)

# thru_tot <- thru %>%
#   group_by(TIME) %>%
#   summarise(ROLLING_HOUR_MVT = sum(ROLLING_HOUR_MVT, na.rm = TRUE)) %>%
#   mutate(PHASE = "Total") %>%
#   ungroup()

thru_max <- thru %>%
  group_by(TIME) %>%
  summarise(ROLLING_HOUR_MVT = mean(PCT99_RHM, na.rm = TRUE)) %>%
  mutate(PHASE = "Peak Service Rate<br>(2016-2023)") %>%
  ungroup()

thru <- thru %>%
  # bind_rows(thru_tot) %>%
  bind_rows(thru_max) %>%
  mutate(TIME = as.character(TIME), TIME = strtrim(TIME, 5))

phase_grps <- c(  "Departures",  "Arrivals",  "Total",  "Peak Service Rate<br>(2016-2023)")

phase_lbl  <- c(  "Departures",  "Arrivals",  "Total",  "Peak Service Rate<br>(2016-2023)")

xax <- list(
  title = "",
  type = "category",
  range = c("00:00", "23:55")
)

yax <- list(
  title = "Average Throughput [flights/hour]",
  titlefont = list(size = 11),
  hoverformat = ".2f"
)

msg <- ifelse(nrow(thru)>0,
              paste0(
                "<b>Average 15-min rolling UTC hour",
                "<br>throughput-weekdays month ",
                pick_mth[thru_mth], " ",
                thru_yr,
                "</b>"),
              paste0("<b>No data available for ", params$icao, "</b>")
)

# if (max(thru$ROLLING_HOUR_MVT) <= 20) {
  msg_y <- 1.1 * max(thru$ROLLING_HOUR_MVT)
# } else {
#   msg_y <- 0.1 * max(thru$ROLLING_HOUR_MVT)
# }


thru <- thru %>%
  mutate(PHASE = factor(PHASE,
                        levels = phase_grps,
                        labels = phase_lbl
  ))


thru %>%
  plot_ly(
    x = ~TIME,
    y = ~ROLLING_HOUR_MVT,
    color = ~PHASE
  ) %>%
  group_by(PHASE) %>%
  add_lines(type = "scatter") %>%
  add_annotations(
    text      = msg,
    x         = 0.5,
    xref      = "paper",
    y         = msg_y,
    showarrow = FALSE,
    ax        = 0,
    ay        = 0,
    align     = "center"
  ) %>%
  layout(
    xaxis       = xax,
    yaxis       = yax,
    hovermode   = "x unified"
    ##############################################################################
    # , annotations = list(
    #   x         = 1,
    #   y         = -0.1,
    #   text      = "Source: Network Manager",
    #   showarrow = FALSE,
    #   xref      = "paper",
    #   yref      = "paper",
    #   xanchor   = "right",
    #   yanchor   = "auto",
    #   xshift    = 0,
    #   yshift    = -35,
    #   font      = list(size = 12)
    # )
    ##############################################################################
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  ) %>% 
  add_download_button(thru %>%
    select(YEAR, MONTH = MONTH_NUM, TIME, PHASE, PEAK_SERVICE_RATE = PCT99_RHM, AVG_THROUGHPUT_RATE = ROLLING_HOUR_MVT), 
    "HOURLY_THROUGHPUT_MM")





# Factsheet figure

phase_lbl2  <- c("Departures", "Arrivals", "Total", "Peak Service Rate\n(2016-2023)")

if (nrow(thru)>0) {
  
  mvts_pm_fig = ggplot(mutate(thru, PHASE = factor(PHASE,
                                                   levels = phase_grps,
                                                   labels = phase_lbl2))) +
    geom_line(aes(x=TIME, y = ROLLING_HOUR_MVT, group = PHASE, color = PHASE), linewidth=linesize_factsheet) +
    theme_factsheet() +
    # theme_bw() +
    # theme(plot.title = element_text(size=100, face="bold", hjust=0.5),
    #       legend.title=element_blank(),
    #       legend.text=element_text(size=100),
    #       legend.position = "bottom",
    #       axis.text=element_text(size=100),
    #       axis.text.x=element_text(angle=270, vjust=0.5),
    #       axis.title=element_text(size=100),
    #       axis.title.x = element_blank(),
    #       plot.margin = unit(c(5.5, 20, 5.5, 60), "pt")) +
    scale_x_discrete(breaks = unique(thru$TIME)[c(TRUE, FALSE)]) +
    labs(x="", y="Average Throughput\n[flights/hour]")
  
} else {
  
  mvts_pm_fig = ggplot() +
    geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), colour='white', fill='white') +
    geom_text(aes(x=0.5, y=0.5,
                  label=paste0("No data\navailable\nfor ", params$icao)), 
              size=60, colour="black") +
    theme_void()
  
}

ggsave(here("media", "factsheet", paste0("Avg_weekday_hourly_throughput_", params$icao, ".pdf")), plot = mvts_pm_fig,
       width = Traffic_layout3[1]*Page_width, height = Traffic_height3, units = "cm", dpi=100, limitsize = FALSE)
