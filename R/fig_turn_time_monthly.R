
TURN_MM <- params$turn_mm %>%
  select(YEAR, MONTH_NUM, TOT_TURN, AC_CLASS, AVG_ACTT, AVG_SDTT) %>%
  mutate(
    LBL_ACTUAL = paste0("Actual Turnaround ", AC_CLASS),
    LBL_SCHED = paste0("Scheduled Turnaround ", AC_CLASS),
    HOVERTEXTACTT = paste0(
      "Class: ", AC_CLASS,
      "<br>Avg. Turn:", round(AVG_ACTT, 2), "min",
      "<br>Nb Turn. :", TOT_TURN
    ),
    HOVERTEXTSDTT = paste0(
      "Class: ", AC_CLASS,
      "<br>Avg. Turn:", round(AVG_SDTT, 2), "min",
      "<br>Nb Turn. :", TOT_TURN
    )
  )

filter_years <- TURN_MM %>%
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


TURN_MM %>%
  plot_ly() %>%
  add_lines(
    x          = ~MONTH_NUM,
    y          = ~AVG_ACTT,
    color      = ~AC_CLASS,
    colors     = turn_col,
    name       = ~LBL_ACTUAL,
    customdata = ~YEAR,
    transforms = list(
      list(
        type      = "filter",
        target    = "customdata",
        operation = "=",
        value     = max_year
      )
    ),
    text          = ~HOVERTEXTACTT,
    hovertemplate = paste0("%{text}"),
    legendgroup   = ~AC_CLASS,
    showlegend    = TRUE
  ) %>%
  add_lines(
    x          = ~MONTH_NUM,
    y          = ~AVG_SDTT,
    color      = ~AC_CLASS,
    colors     = turn_col,
    name       = ~LBL_SCHED,
    line       = list(dash = "dash"),
    customdata = ~YEAR,
    transforms = list(
      list(
        type      = "filter",
        target    = "customdata",
        operation = "=",
        value     = max_year
      )
    ),
    text          = ~HOVERTEXTSDTT,
    hovertemplate = paste0("%{text}"),
    legendgroup = ~AC_CLASS,
    showlegend = TRUE
  ) %>%
  layout(
    updatemenus = list(button_type_list),
    hovermode   = "x unified",
    xaxis       = tick_yr_no_title,
    yaxis       = list(
      title     = "Average Turnaround times [min/flight]",
      titlefont = list(size = 11)
    )
    ##############################################################################
    # ,annotations = list(
    #   x         = 1,
    #   y         = -0.1,
    #   text      = "Source: APDF",
    #   showarrow = FALSE,
    #   xref      = "paper",
    #   yref      = "paper",
    #   xanchor   = "right",
    #   yanchor   = "auto",
    #   xshift    = 0,
    #   yshift    = -10,
    #   font      = list(size = 12)
    # )
    ##############################################################################
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  ) %>% 
  add_download_button(
    TURN_MM %>%
      select(
        YEAR,
        MONTH = MONTH_NUM,
        AC_CLASS,
        AVG_ACTUAL_TT = AVG_ACTT, 
        AVG_SCHED_TT = AVG_SDTT), "TURNAROUND_MM")





# Factsheet figure

TURN_MM_curr_year=filter(TURN_MM, YEAR==max_year) %>% 
  mutate(Month=factor(month.abb[MONTH_NUM], levels = month.abb)) %>%
  tidyr::pivot_longer(
    cols      = starts_with("AVG_"),
    names_to  = "Metric",
    values_to = "TT"
  ) %>% 
  mutate(Label=ifelse(substr(Metric, 5, 8)=="ACTT", LBL_ACTUAL, LBL_SCHED)) %>% 
  arrange(YEAR, Month, Label)

TURN_MM_fig = ggplot(data=filter(TURN_MM_curr_year, YEAR==max_year)) +
  geom_line(aes(x=Month, y=TT, colour=Label, group=Label, linetype=Label), size=linesize_factsheet) +
  scale_colour_manual(values=rep(turn_col, 2),
                      labels=c("Actual Turnaround H", "Actual Turnaround MJ", "Actual Turnaround MT", 
                               "Scheduled Turnaround H", "Scheduled Turnaround MJ", "Scheduled Turnaround MT")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dashed", "dashed", "dashed"),
                        labels=c("Actual Turnaround H", "Actual Turnaround MJ", "Actual Turnaround MT", 
                                 "Scheduled Turnaround H", "Scheduled Turnaround MJ", "Scheduled Turnaround MT")) +
  labs(colour="", linetype="") +
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
  labs(x="", y="Average Actual Turnaround time (min/flight)\n")
ggsave(here("media", "factsheet", paste0("Turn_Time_Monthly_", params$icao, ".pdf")), plot=TURN_MM_fig, 
       width = Turnaround_times_layout1[2]*Page_width, height = Turnaround_times_height1, units = "cm", dpi=100, 
       limitsize = FALSE)

