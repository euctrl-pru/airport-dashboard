
TURN_YY <- params$turn_yy %>%
  select(YEAR, AC_CLASS, AVG_ACTT)

turn_col <- c(
  "#C6E0B4", # RGB 198,224,180 # Heavy
  "#F8CBAD", # RGB 248,203,173 # Medium Jet
  "#BDD7EE"
) # RGB 189,215,238 # Medium Turboprop

TURN_YY$AC_CLASS <- factor(TURN_YY$AC_CLASS,
                           levels = c("H", "MJ", "MT"),
                           labels = c("Heavy", "Medium Jet", "Medium Turboprop")
)

TURN_YY <- TURN_YY %>%
  mutate(YEAR = as.character(YEAR))


TURN_YY %>%
  plot_ly(
    x      = ~YEAR,
    y      = ~AVG_ACTT,
    type   = "bar",
    color  = ~AC_CLASS,
    colors = turn_col
  ) %>%
  layout(
    barmode   = "group",
    hovermode = "x unified",
    xaxis     = list(title = ""),
    yaxis     = list(
      title       = "Average Actual Turnaround time [min/flight]",
      titlefont   = list(size = 11),
      hoverformat = ".2f"
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
    #   yshift    = -5,
    #   font      = list(size = 12)
    # )
    ##############################################################################
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  )





# Factsheet figure

TURN_YY_fig = ggplot(data=TURN_YY) +
  geom_bar(aes(x=YEAR, y=AVG_ACTT, fill=AC_CLASS), stat="identity", position = "dodge") +
  scale_fill_manual(values=turn_col) +
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
ggsave(here("media", "factsheet", paste0("Turn_Time_Yearly_", params$icao, ".pdf")), plot=TURN_YY_fig, 
       width = Turnaround_times_layout1[1]*Page_width, height = Turnaround_times_height1, units = "cm", dpi=100, 
       limitsize = FALSE)

